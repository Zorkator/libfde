#include <config.h>

# define _CRT_SECURE_NO_WARNINGS
#include <map>
#include <errno.h>
#include <stdio.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/plugin.hpp"
#include "fortres/dirent.hpp"
#include "fortres/exception.hpp"
#include "fortres/auto_ptr.hpp"
#include "fortres/String.hpp"
#include "fortres/stdlib.h"

#if defined HAVE_DLFCN_H
# include <dlfcn.h>

# define dlOpen(lib)        dlopen( lib, RTLD_NOW | RTLD_GLOBAL )
# define dlClose(hdl)       dlclose( hdl )
# define dlSym(hdl,sym)     dlsym( hdl, sym )
# define dlError()          dlerror()
# define dlErrorCode()      errno

#elif defined HAVE_LIBLOADERAPI_H
# include <windows.h>
# include <libloaderapi.h>

# define dlOpen(lib)        LoadLibraryA( (LPCSTR)lib )
# define dlClose(hdl)       FreeLibrary( (HMODULE)hdl )
# define dlSym(hdl,sym)     GetProcAddress( (HMODULE)hdl, sym )
# define dlError()          "not yet implemented!"
# define dlErrorCode()      GetLastError()

#else
  #error "Neither HAVE_LIBLOADERAPI_H nor HAVE_DLFCN_H"
#endif

# define _ref_str(strRef)       strRef->str()
# define _ref_cstr(strRef)      strRef->str().c_str()
# define _ref_str_chk(strRef)   (strRef? strRef->str() : std::string())
# define _ref_cstr_chk(strRef)  (strRef? strRef->str().c_str() : "")

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


/*******************/
class PluginBroker
/*******************/
{
  public:
    typedef enum
    {
      State_invalid  = -1,
      State_disabled =  0,
      State_enabled  =  1,
      State_located  =  2
    /*******************/
    } PluginState;
    /*******************/


  private:
    /*******************/
    class Predicate
    /*******************/
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


    /*******************/
    class PluginRef
    /*******************/
    {
      public:
        PluginRef( void )
        : state(State_invalid)
          { /* nothing to do here */ }

        PluginRef( const char *filePath_, const SharedLib::Handle &handle_, PluginState state_ = State_disabled )
        : handle(handle_)
        {
          if (state_ == State_located)
          {
            /* use original filePath if shared library should be located by OS (PATH / LD_LIBRARY_PATH) */
            state    = State_enabled;
            filePath = filePath_;
          }
          else
          {
            /* use original path if realpath conversion failed */
            state    = state_;
            filePath = realpath_of( filePath_ );
            if (filePath.empty())
              { filePath = filePath_; }
          }
        }

        String            filePath;
        SharedLib::Handle handle;
        PluginState       state;
    };


    /***************/
    class Map
    /***************/
    : public std::map<String, PluginRef>
    {
      public:
        typedef std::map<String, PluginRef>::iterator  Iterator;
        typedef SharedLib::Function                    Initializer;

        static SharedLib *
          tryLoad( const String &filePath, const Predicate &pred = Predicate() )
          {
            SharedLib        *ptr = NULL;
            SharedLib::Handle lib( new SharedLib( filePath.c_str() ) );

            if (_dbg_info)
              { fprintf( stdout, "FORTRES: check plugin '%s' ... ", filePath.c_str() ); }
            if (pred( lib.get() ))
              { ptr = lib.release(); }
            return ptr;
          }


        SharedLib *
          getPlugin( const String &id, const String &envPath )
          {
            SharedLib *lib = NULL;
            Iterator   itr = this->find( id );

            if (itr != this->end())
            {
              PluginRef &ref = itr->second;
              if (ref.state == State_enabled)
              {
                /* plugin enabled */
                lib = ref.handle.get();
                if (lib == NULL)
                {
                  /* plugin not yet loaded ... */
                  this->pushEnvPath( envPath );
                  lib = this->tryLoad( ref.filePath ); //< default Predicate is sufficient here!
                  this->popEnvPath();

                  ref.handle.reset( lib );
                  if (lib)
                  {
                    /* if implemented, call initialize function */
                    Initializer init = (Initializer)lib->getSymbol("initialize_c_");
                    if (init)
                      { init(); }
                  }
                }
              }
            }
            return lib;
          }


        void
          insertPlugin( const String &filePath
                      , const String &id = String()
                      , const SharedLib::Handle &lib = SharedLib::Handle()
                      , PluginState state = State_disabled )
          {
            const String &pluginId = (id.empty())? libFileToId( filePath ) : id;
            (*this)[pluginId] = PluginRef( filePath.c_str(), lib, state );
          }


        void
          pushEnvPath( const std::string &envPath )
          {
            _envBuff = getenv( LIB_PATH_VAR );
            setenv( LIB_PATH_VAR, (envPath + _envBuff).c_str(), 1 /*<< override */ );
          }


        void
          popEnvPath( void )
            { setenv( LIB_PATH_VAR, _envBuff.c_str(), 1 /*<< override */ ); }

      private:
        String _envBuff;
    };

  public:
      PluginBroker( const StringRef *pluginDir = NULL, const StringRef *libPath = NULL )
      : _pluginDir(_ref_str_chk(pluginDir)), _libPath(_ref_str_chk(pluginDir))
      {
        if (!_pluginDir.endsWith("/"))
          { _pluginDir.append("/"); }
        if (libPath && libPath->length())
          { _libPath << LIB_PATH_SEP << _ref_cstr_chk(libPath);  }
        _libPath << LIB_PATH_SEP;
      }


    void
      registerPlugin( const StringRef *filePath, const StringRef *id, PluginState state = State_enabled )
        { _pluginMap.insertPlugin( _ref_str(filePath), _ref_str(id), NULL, state ); }


    bool
      setEnabled( const StringRef *pluginId, bool isEnabled = true )
      {
        Map::Iterator itr  = _pluginMap.find( _ref_str(pluginId) );
        bool          isOk = (itr != _pluginMap.end());
        if (isOk)
          { itr->second.state = (isEnabled)? State_enabled : State_disabled; }
        return isOk;
      }


    void
      scanPlugins( const StringRef *predSym )
      {
        struct dirent *entry;
        DIR           *dir = opendir( _pluginDir.c_str() );

        if (dir != NULL)
        {
          _pluginMap.pushEnvPath( _libPath );
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
          _pluginMap.popEnvPath();
        }
      }


    void *
      getSymbolOf( const StringRef *pluginId, const StringRef *symId )
      {
        SharedLib *lib = _pluginMap.getPlugin( _ref_str(pluginId), _libPath );
        void      *sym = NULL;

        if (lib)
          { sym = lib->getSymbol( _ref_cstr(symId) ); }
        return sym;
      }


    bool
      isAvailable( const StringRef *pluginId, const StringRef *symId = NULL )
      {
        SharedLib *lib   = _pluginMap.getPlugin( _ref_str(pluginId), _libPath );
        bool       avail = false;

        if (lib)
          { avail = (symId == NULL || symId->length() == 0 || lib->getSymbol( _ref_cstr(symId) )); }
        return avail;
      }


    SharedLib *
      operator [] ( const StringRef *id )
        { return _pluginMap.getPlugin( _ref_str(id), _libPath ); }


    void
      iterPlugins( PluginInfoHandler handler )
      {
        StringRef id, filePath;
        for (Map::Iterator itr = _pluginMap.begin(); itr != _pluginMap.end(); ++itr)
        {
          PluginRef &ref   = itr->second;
          handler( &id.referTo( itr->first ), &filePath.referTo( ref.filePath ), (int *)&ref.state );
        }
      }


    static String
      libFileToId( const String &libFile )
      {
        size_t id_beg, id_end;

        id_beg = libFile.find_last_of( PATH_SEP PATH_SEP_OTHER );
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
    String _libPath;
};



inline PluginBroker *
getBroker( const StringRef *pluginDir = NULL, const StringRef *libPath = NULL )
{
  static auto_ptr<PluginBroker> broker;
  if (pluginDir)
    { broker = new PluginBroker( pluginDir, libPath ); }
  return broker.get();
}


_dllExport_C
void
f_plugin_set_path( StringRef *path, StringRef *libPath, StringRef *chkSym )
{
  if (_dbg_info)
    { fprintf( stdout, "FORTRES: setting plugin path\n >> %s\n", _ref_cstr(path) ); }
  getBroker( path, libPath )->scanPlugins( chkSym );
}


_dllExport_C
void
f_plugin_register( StringRef *filePath, StringRef *id, int *isEnabled )
{
  bool enabled = isEnabled == NULL || *isEnabled != 0; //< default: true!
  getBroker()->registerPlugin( filePath, id, PluginBroker::PluginState(enabled) );
}


_dllExport_C
void
f_plugin_register_so( StringRef *fileName, StringRef *id )
{
  getBroker()->registerPlugin( fileName, id, PluginBroker::State_located );
}


_dllExport_C
int
f_plugin_set_enabled( StringRef *pluginId, int *isEnabled )
{
  bool enabled = isEnabled == NULL || *isEnabled != 0; //< default: true!
  return getBroker()->setEnabled( pluginId, enabled );
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
  id->concat( PluginBroker::libFileToId( filePath->str() ) ).pad();
}


_dllExport_C
int
f_plugin_available( StringRef *pluginId, StringRef *symId )
{
  return getBroker()->isAvailable( pluginId, symId );
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

