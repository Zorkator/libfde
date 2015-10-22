
#include "StringRef.hpp"

#include <map>

#if defined _MSC_VER
# include <windows.h>
# define LIB_PREFIX         ""
# define LIB_SUFFIX         ".dll"
# define PATH_DELIM         "\\"

# define dlOpen(lib)        LoadLibrary( (LPCSTR)lib )
# define dlClose(hdl)       FreeLibrary( (HMODULE)hdl )
# define dlProc(hdl,proc)   GetProcAddress( (HMODULE)hdl, proc )
# define dlError()          "not yet implemented!"
# define _dllExport         __declspec(dllexport)

#else
# include <dlfcn.h>
# define LIB_PREFIX         "lib"
# define LIB_SUFFIX         ".so"
# define PATH_DELIM         "/"

# define dlOpen(lib)        dlopen( lib, RTLD_NOW | RTLD_GLOBAL )
# define dlClose(hdl)       dlclose( hdl )
# define dlProc(hdl,proc)   dlsym( hdl, proc )
# define dlError()          dlerror()
# define _dllExport         
#endif


class SharedLib
{
  public:
      SharedLib( const char *libFileName )
      : _hdl(NULL)
        { _hdl = dlOpen( libFileName  ); }

    virtual
      ~SharedLib( void )
      {
        if (_hdl != NULL)
          { dlClose( _hdl ); }
      }

    const char *
      getError( void ) const
        { return dlError(); }

    bool
      isLoaded( void ) const
        { return _hdl != NULL; }

  private:
    void *_hdl;
};



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
      

  public:
    typedef std::map<std::string, SharedLib *>  LibraryMap;
    
    SharedLib *
      operator [] ( const char *libId )
      {
        LibraryMap::iterator it = _libMap.find( libId );
        if (it != _libMap.end()) return it->second;
        else                     return NULL;
      }

  private:
    LibraryMap _libMap;
};


// extern "C" _dllExport
// f_scan

