
#include <map>
#include <errno.h>
#include <stdio.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/sharedlib.hpp"
#include "fortres/dirent.hpp"
#include "fortres/exception.hpp"
#include "fortres/auto_ptr.hpp"
#include "fortres/String.hpp"
#include "fortres/stdlib.h"

#if defined _MSC_VER
# include <windows.h>
# define LIB_PREFIX         "lib"
# define LIB_SUFFIX         ".dll"
# define LIB_PATH_VAR       "PATH"
# define LIB_PATH_SEP       ";"
# define LIB_DIR_SEP        "\\"
# define LIB_DIR_SEP_other  "/"

# define dlOpen(lib)        LoadLibraryA( (LPCSTR)lib )
# define dlClose(hdl)       FreeLibrary( (HMODULE)hdl )
# define dlSym(hdl,sym)     GetProcAddress( (HMODULE)hdl, sym )
# define dlError()          "not yet implemented!"
# define dlErrorCode()      GetLastError()

#else
# include <dlfcn.h>
# define LIB_PREFIX         "lib"
# define LIB_SUFFIX         ".so"
# define LIB_PATH_VAR       "LD_LIBRARY_PATH"
# define LIB_PATH_SEP       ":"
# define LIB_DIR_SEP        "/"
# define LIB_DIR_SEP_other  "\\"

# define dlOpen(lib)        dlopen( lib, RTLD_NOW | RTLD_GLOBAL )
# define dlClose(hdl)       dlclose( hdl )
# define dlSym(hdl,sym)     dlsym( hdl, sym )
# define dlError()          dlerror()
# define dlErrorCode()      errno
#endif

# define _ref_str(strRef)       strRef->str()
# define _ref_cstr(strRef)      strRef->str().c_str()

using namespace fortres;

static String _libPrefix( LIB_PREFIX );
static String _libSuffix( LIB_SUFFIX );
static bool   _dbg_info = (getenv("FORTRES_DEBUG") != NULL);

class SharedLib
{
  public:
      typedef auto_ptr<SharedLib>  Handle;
      typedef void (*Function)( void );

      SharedLib( const char *libFile )
      {
        _hdl = dlOpen( libFile );
        if (!_hdl && _dbg_info)
          { fprintf( stderr, "ERROR loading SharedLib %s\n >> [Code %d] %s\n", libFile, dlErrorCode(), dlError() ); }
      }

    virtual
      ~SharedLib( void )
      {
        if (_hdl != NULL)
          { dlClose( _hdl ); }
      }

    void *
      getSymbol( const char *funcID )
        { return dlSym( _hdl, funcID ); }

    const char *
      getError( void ) const
        { return dlError(); }

    int
      getErrorCode( void ) const
        { return dlErrorCode(); }

      operator bool ( void ) const
        { return (_hdl != NULL); }

  private:
    void *_hdl;
};


class PluginBroker
{
    class Predicate
    {
      public:
          Predicate( const char *sym = "" ) : _sym(sym) { /* empty*/ }

        bool
          operator () ( SharedLib *lib ) const
          {
            const char *sym    = _sym.c_str();
            bool        st_lib = (lib && *lib);
            bool        st_sym = (st_lib && (_sym.empty() || lib->getSymbol(sym)));

            if (_dbg_info)
            {
              fprintf( stdout, "%s, ", (st_lib)? "loaded" : "load failed" );
              fprintf( stdout, "try symbol %s: %s\n", sym, (st_sym)? "ok" : "not found" );
            }
            return st_sym;
          }

      private:
        String _sym;
    };


    typedef std::pair<String, SharedLib::Handle>  Value;

    class Map
    : public std::map<String, Value>
    {
      public:
        typedef std::pair<String, Value>              Item;
        typedef std::map<String, Value>::iterator     Iterator;
        typedef SharedLib::Function                   Initializer;

        bool
          hasPlugin( const String &id ) const
            { return (this->find(id) != this->end()); }

        SharedLib *
          getPlugin( const String &id )
          {
            SharedLib *lib = NULL;
            Iterator   itr = this->find( id );

            if (itr != this->end())
            {
              String            &filePath = itr->second.first;
              SharedLib::Handle &libHdl   = itr->second.second;
              if ((lib = libHdl.get()) == NULL)
              {
                lib = this->tryLoad( filePath ); //< default Predicate is sufficient here!
                libHdl.reset( lib );
                if (lib)
                {
                  Initializer init = (Initializer)lib->getSymbol("initialize_c_");
                  if (init)
                    { init(); }
                }
              }
            }
            return lib;
          }

        static SharedLib *
          tryLoad( const String &id, const Predicate &pred = Predicate() )
          {
            SharedLib        *ptr = NULL;
            SharedLib::Handle lib( new SharedLib( id.c_str() ) );

            if (_dbg_info)
              { fprintf( stdout, "FORTRES: check plugin '%s' ... ", id.c_str() ); }
            if (pred( lib.get() ))
              { ptr = lib.release(); }
            return ptr;
          }

        void
          insertPlugin( const String &filePath, const String &id = String()
                      , const SharedLib::Handle &lib = SharedLib::Handle() )
          {
            const String &pluginId = (id.empty())? libFileToId( filePath ) : id;
            (*this)[pluginId] = Value( realpath_of(filePath.c_str()), lib );
          }
    };

  public:
      PluginBroker( const StringRef *path = NULL )
      {
        if (path != NULL)
        {
          _pluginDir = _ref_str(path);
          if (!_pluginDir.empty())
            { _pluginDir.append("/"); }
        }
      }

    void
      registerPlugin( const StringRef *filePath )
        { _pluginMap.insertPlugin( _ref_str(filePath) ); }

    void
      scanPlugins( const StringRef *predSym )
      {
        struct dirent *entry;
        DIR           *dir = opendir( _pluginDir.c_str() );

        if (dir != NULL)
        {
          while ((entry = readdir( dir )) != NULL)
          {
            String filePath( _pluginDir + entry->d_name );

            if (filePath.endsWith( _libSuffix ))
            {
              SharedLib::Handle lib( Map::tryLoad( filePath, Predicate(_ref_cstr(predSym)) ) );
              if (lib.get())
                { _pluginMap.insertPlugin( filePath ); }
            }
          }
          closedir( dir );
        }
      }

    void *
      getSymbolOf( const StringRef *pluginId, const StringRef *symId )
      {
        SharedLib *lib = _pluginMap.getPlugin( _ref_str(pluginId) );
        void      *sym = NULL;

        if (lib)
          { sym = lib->getSymbol( _ref_cstr(symId) ); }
        return sym;
      }
    
    SharedLib *
      operator [] ( const StringRef *id )
        { return _pluginMap.getPlugin( _ref_str(id) ); }

    void
      iterPlugins( PluginInfoHandler handler )
      {
        StringRef id, filePath;
        for (Map::Iterator itr = _pluginMap.begin(); itr != _pluginMap.end(); ++itr)
          { handler( &id.referTo( itr->first ), &filePath.referTo( itr->second.first ) ); }
      }

    static String
      libFileToId( const String &libFile )
      {
        size_t id_beg, id_end;

        id_beg = libFile.find_last_of( LIB_DIR_SEP LIB_DIR_SEP_other );
        id_beg = (id_beg == String::npos)? 0 : id_beg + 1; //< skip DIR_SEP
        if (libFile.find( _libPrefix, id_beg ) == id_beg)  //< skip optional LIB_PREFIX 'lib'
          { id_beg += _libPrefix.length(); }

        id_end = libFile.find( '.', id_beg );
        id_end = (id_end == String::npos)? libFile.length() : id_end;
        return libFile.substr( id_beg, id_end - id_beg );
      }
      
  private:
    Map    _pluginMap;
    String _pluginDir;
};



inline PluginBroker *
getBroker( const StringRef *pluginDir = NULL )
{
  static auto_ptr<PluginBroker> broker;
  if (pluginDir)
    { broker = new PluginBroker( pluginDir ); }
  return broker.get();
}


_dllExport_C
void
f_plugin_set_path( StringRef *path, StringRef *libPath, StringRef *chkSym )
{
  if (_dbg_info)
    { fprintf( stdout, "FORTRES: setting plugin path\n >> %s\n", _ref_cstr(path) ); }
  if (libPath->length())
  {
    String libPathStr( libPath->str() );
    String envStr( getenv(LIB_PATH_VAR) );
    if (!envStr.startsWith( libPathStr ))
    {
      libPathStr.append( LIB_PATH_SEP ).append( envStr );
      setenv( LIB_PATH_VAR, libPathStr.c_str(), 1 /*<< override */);
      if (_dbg_info)
        { fprintf( stdout, "FORTRES: setting environment variable\n >> %s = %s\n", LIB_PATH_VAR, libPathStr.c_str() ); }
    }
  }
  getBroker( path )->scanPlugins( chkSym );
}


_dllExport_C
void
f_plugin_register( StringRef *filePath )
{
  getBroker()->registerPlugin( filePath );
}


_dllExport_C
void
f_plugin_iterate( PluginInfoHandler handler )
{
  getBroker()->iterPlugins( handler );
}


_dllExport_C
void
f_plugin_filePath_to_id( StringRef *filePath, StringRef *id )
{
  *id = PluginBroker::libFileToId( filePath->str() );
}


_dllExport_C
void *
f_plugin_sym( StringRef *pluginId, StringRef *symId )
{
  void *sym = getBroker()->getSymbolOf( pluginId, symId );
  if (sym == NULL)
  {
    std::string *msg = new std::string( _ref_str(pluginId) + "::" + _ref_str(symId) );
    f_throw_str( NotImplementedError, &msg );
  }
  return sym;
}


_dllExport_C
void
f_plugin_call( StringRef *pluginId, StringRef *symId )
{
  void *func = getBroker()->getSymbolOf( pluginId, symId );
  if (func == NULL)
  {
    std::string *msg = new std::string( _ref_str(pluginId) + "::" + _ref_str(symId) );
    f_throw_str( NotImplementedError, &msg );
  }
  reinterpret_cast<SharedLib::Function>(func)();
}


_dllExport_C
void *
f_plugin_try_sym( StringRef *pluginId, StringRef *symId )
{
  return getBroker()->getSymbolOf( pluginId, symId );
}


_dllExport_C
int
f_plugin_try_call( StringRef *pluginId, StringRef *symId )
{
  void *func = getBroker()->getSymbolOf( pluginId, symId );
  if (func != NULL)
    { reinterpret_cast<SharedLib::Function>(func)(); }
  return (func != NULL);
}

