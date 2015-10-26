
#include "fortres/StringRef.hpp"
#include "fortres/dirent.h"
#include <map>
#include <memory>
#include <iostream>

#if defined _MSC_VER
# include <windows.h>
# define LIB_PREFIX         ""
# define LIB_SUFFIX         ".dll"

# define dlOpen(lib)        LoadLibrary( (LPCSTR)lib )
# define dlClose(hdl)       FreeLibrary( (HMODULE)hdl )
# define dlSym(hdl,sym)     GetProcAddress( (HMODULE)hdl, sym )
# define dlError()          "not yet implemented!"

#else
# include <dlfcn.h>
# define LIB_PREFIX         "lib"
# define LIB_SUFFIX         ".so"

# define dlOpen(lib)        dlopen( lib, RTLD_NOW | RTLD_GLOBAL )
# define dlClose(hdl)       dlclose( hdl )
# define dlSym(hdl,sym)     dlsym( hdl, sym )
# define dlError()          dlerror()
#endif


class SharedLib
{
  public:
      typedef std::auto_ptr<SharedLib>  Handle;

      SharedLib( const char *libFile )
      : _libFile(libFile)
        { _hdl = dlOpen( libFile ); }

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

      operator bool ( void ) const
        { return (_hdl != NULL); }

    const std::string &
      libFile( void ) const
        { return _libFile; }

  private:
    void       *_hdl;
    std::string _libFile;
};


static std::string _libPrefix( LIB_PREFIX );
static std::string _libSuffix( LIB_SUFFIX );

class PluginBroker
{
    static std::string
      getLibNameById( const char *libId, const char *path = "" )
      {
        std::string libName(path);
        if (!libName.empty())
          { libName.append( PATH_DELIM ); }
        return libName.append( LIB_PREFIX ).append( libId ).append( LIB_SUFFIX );
      }
      

    class PluginMap
    : public std::map<std::string, SharedLib::Handle>
    {
      public:
        typedef std::map<std::string, SharedLib::Handle>  MapType;
        typedef MapType::iterator                         Iterator;

        bool
          hasPlugin( const char *id ) const
           { return (this->find(id) != this->end()); }

        SharedLib *
          getPlugin( const char *id )
          {
            SharedLib *lib = NULL;
            Iterator   itr = this->find( id );

            if (itr != this->end())
            {
              if ((lib = itr->second.get()) == NULL)
              {
                lib = this->tryLoad( id );
                (*this)[id] = SharedLib::Handle(lib);
              }
            }
            return lib;
          }

        static SharedLib *
          tryLoad( const char *id )
          {
            SharedLib        *ptr = NULL;
            SharedLib::Handle lib( new SharedLib( id ) );

            if (*lib)// && lib->getSymbol("cookie"))
              { ptr = lib.release(); }
            return ptr;
          }
    };

  public:
      PluginBroker( const StringRef *path = NULL )
      {
        if (path != NULL)
        {
          _pluginDir = path->str();
        }
      }

    void
      registerPlugin( const char *id, SharedLib::Handle lib = SharedLib::Handle() )
        { _pluginMap[id] = lib; }

    void
      scanPlugins( void )
      {
        struct dirent *entry;
        DIR           *dir = opendir( _pluginDir.c_str() );

        if (dir != NULL)
        {
          while ((entry = readdir( dir )) != NULL)
          {
            SharedLib::Handle lib( PluginMap::tryLoad( entry->d_name ) );
            if (lib.get())
            {
              // make id from fileName ...
              std::string id( entry->d_name );
              this->registerPlugin( id.c_str() ); // << provide parameter lib for preloaded plugins
            }
          }
          closedir( dir );
        }
      }
  
    
    SharedLib *
      operator [] ( const char *id )
       { return _pluginMap.getPlugin( id ); }

  private:
    PluginMap   _pluginMap;
    std::string _pluginDir;
};



inline PluginBroker &
getBroker( const StringRef *pluginDir = NULL )
{
  static PluginBroker broker( pluginDir );
  return broker;
}


_dllExport_C
void
f_set_plugin_path( StringRef *path, int scan )
{
  PluginBroker &broker = getBroker( path );
  if (scan)
    { broker.scanPlugins(); }
}


_dllExport_C
void
f_register_plugin( StringRef *pluginId )
{
  getBroker().registerPlugin( pluginId->str().c_str() );
}

