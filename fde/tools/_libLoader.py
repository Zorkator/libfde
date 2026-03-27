
from ctypes     import CDLL as _CDLL, c_int64
from os         import environ as _env, pathsep as _pathDelim, path as _path, getpid as _getpid
from glob       import glob
from contextlib import contextmanager
import logging
import platform

_isWin   = platform.system() == "Windows"
_PATH    = ('LD_LIBRARY_PATH', 'PATH')[_isWin]
_libPtrn = (['lib%s.so', 'lib%s.so*'],
            ['*%s.dll',  '*%s.*.dll'])[_isWin]

if _isWin:
    from ctypes import windll
    freeLibrary = windll.kernel32.FreeLibrary
else:
    from ctypes import cdll, util
    freeLibrary = cdll.LoadLibrary( util.find_library('dl') ).dlclose


@contextmanager
def extendLoadPaths( *pathpattern, **opts ):
    from itertools import chain
    envPaths = _env.get( _PATH, '' )
    try:
        paths    = [p for p in chain( *(glob( pat ) for pat in pathpattern) ) if _path.isdir( p )]
        _env[_PATH] = _pathDelim.join( (*paths, envPaths) )
        yield None
    finally:
        if envPaths and opts.get('restore', True):
            _env[_PATH] = envPaths


def libPattern( name, versionTagged = False ):
    return _libPtrn[versionTagged] % name


logging.basicConfig()

#-------------------------------------------
class CDLL_t( _CDLL ):
#-------------------------------------------

    def __init__( self, name, **kwArgs ):
        try             : super(CDLL_t, self).__init__( name, **kwArgs )
        except Exception: super(CDLL_t, self).__init__( name, **dict( {'winmode':0}, **kwArgs ) )

    def __getitem__( self, ident ):
        """if given more than one argument, try one after another before giving up and returning the last as default."""
        if isinstance( ident, tuple ):
            for i in ident[:-1]:
                try                  : return super(CDLL_t, self).__getitem__( i )
                except AttributeError: pass
            return ident[-1]
        else:
            return super(CDLL_t, self).__getitem__( ident )

    def unload( self ):
        freeLibrary( c_int64(self._handle) )


#-------------------------------------------
class LibLoader( object ):
#-------------------------------------------

    class Success( Exception ):
        pass

    @staticmethod
    def splitEnvPaths( envVarId ):
        return [p for p in _env.get( envVarId, '' ).split( _pathDelim ) if p]

    def opt( self, id, default='' ):
        return self._opt.get( id, default )


    def searchpathIter( self ):
        from ..        import __path__ as parent_path
        from sysconfig import get_path
        pathList = ['.']
        pathList.extend( self.splitEnvPaths( self.opt( 'prioPathEnv' ) ) )
        pathList.append( parent_path[0] )
        pathList.append( get_path('purelib') )
        pathList.extend( self.splitEnvPaths( _PATH ) )
        return iter( pathList )


    @property
    def explicitFilePath( self ):
        """return explicit filePath setting, either by argument or environment variable."""
        return self.opt( 'filePath' ) or _env.get( self.opt( 'fileEnv' ) )

    @property
    def relativeFilePath( self ):
        """return relative filePath setting, applicable to search paths."""
        return self.opt( 'libPattern' )


    @property
    def handle( self ):
        try   : return self._hdl
        except:
            if self.opt( '--debug' ):
                from . import debug; debug()

            try:
                searchPaths = self.splitEnvPaths( self.opt( 'prioPathEnv' ) )
                filePath    = self.explicitFilePath
                if filePath:
                    # try loading for explicit setting
                    filePath = self._tryMatch( filePath )   #< try matching with already loaded libraries
                    self._tryLoad( filePath, searchPaths )
                else:
                    # no explicit filePath given ... so try via libPattern and search paths
                    filePath   = self.relativeFilePath
                    matchedLib = self._tryMatch( filePath ) #< try matching with already loaded libraries
                    # try loading matched lib ...
                    self._tryLoad( matchedLib, searchPaths )
                    # ... before searching in search-paths
                    for path in self.searchpathIter():
                        self._tryLoad( path + _path.sep + filePath, searchPaths )

                raise OSError( "unable to load shared library {0}".format( filePath ) )

            except self.Success:
                self._log.info( "loaded shared library {0}\n".format( self._hdl._name ) )
            return self._hdl


    def _tryLoad( self, libPattern, searchPaths ):
        with extendLoadPaths( _path.dirname( libPattern ), *searchPaths, restore=self.opt('restorePATH') ):
            self.__dict__.pop( '_hdl', None )
            self._log.debug( "try loading %s" % libPattern )
            for f in glob( str(libPattern) ):
                self._log.debug( "\ttry " + str(f) )
                try   : self._hdl = CDLL_t( str(f) ); break  # < break if load succeeded
                except: self._opt['onLoadError']( str(f) )

            if getattr( self, '_hdl', None ):
                # if loader has a named environment variable for explicit filePath
                #   we update the environment variable to allow child processes loading the same library.
                if self.opt('fileEnv'):
                    _env[ self.opt('fileEnv') ] = self._hdl._name
                raise self.Success


    def _tryMatch( self, libPattern ):
        if libPattern and self.opt( 'matchExisting' ):
            try               : import psutil, fnmatch
            except ImportError:
                self._log.warn( "library matching not available, need packages psutil and fnmatch!\n" )
            else:
                pattern = _path.normpath( ('*/', '')[_path.isabs( libPattern )] + str(libPattern) )
                self._log.debug( "try matching " + pattern )
                for lib in psutil.Process( _getpid() ).memory_maps():
                    if fnmatch.fnmatch( lib.path, pattern ):
                        libPattern = lib.path
                        self._log.info( "\tmatched already loaded library {0}".format( libPattern ) )
                        break
        return libPattern


    def __init__( self, filePath = None, name = None, versionTagged = True, **kwArgs ):
        self._opt = dict( onLoadError=lambda f: None )
        self._log = logging.getLogger( type(self).__name__ )
        if   filePath                  : kwArgs.update( filePath=filePath )
        elif 'libPattern' not in kwArgs: kwArgs.update( libPattern=libPattern(name, versionTagged) )
        kwArgs.setdefault( 'logLevel', 'ERROR' )
        kwArgs.setdefault( 'restorePATH', True )
        kwArgs.setdefault( 'matchExisting', True )
        self.set( **kwArgs )

    def __str__( self ):
        try   : return self._hdl._name
        except: return self.explicitFilePath or self.relativeFilePath

    def set( self, **kwArgs ):
        try            : self._log.setLevel( kwArgs['logLevel'] )
        except KeyError: pass
        self._opt.update( kwArgs )


core_loader = LibLoader( name='fde', fileEnv='LIBFDE', prioPathEnv='FDEPATH' )
