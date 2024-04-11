
from ctypes import CDLL as _CDLL
from os     import environ as _env, pathsep as _pathDelim, path as _path, getpid as _getpid
from glob   import glob
from sys    import stdout
import logging
import platform

_isWin = platform.system() == "Windows"
_PATH  = ('LD_LIBRARY_PATH', 'PATH')[_isWin]

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
        envPaths = _env.get( _PATH, '' )
        paths    = list( filter( _path.isdir, glob( _path.dirname( libPattern ) ) ) ) + searchPaths
        paths.append( envPaths )

        _env[_PATH] = _pathDelim.join( paths )
        self.__dict__.pop( '_hdl', None )
        for f in glob( libPattern ):
            self._log.debug( "try loading " + str( f ) )
            try   : self._hdl = CDLL_t( str( f ) ); break  # < break if load succeeded
            except: pass
        _env[_PATH] = envPaths

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
                prefix = ('*/', '')[_path.isabs( libPattern )]
                p = psutil.Process( _getpid() )
                for lib in p.memory_maps():
                    if fnmatch.fnmatch( lib.path, _path.normpath( prefix + str( libPattern ) ) ):
                        libPattern = lib.path
                        self._log.info( "matched already loaded library {0}".format( libPattern ) )
                        break
        return libPattern


    def __init__( self, **kwArgs ):
        self._opt = dict()
        self._log = logging.getLogger( type(self).__name__ )
        kwArgs.setdefault( 'logLevel', 'ERROR' )
        self.set( **kwArgs )

    def __str__( self ):
        try   : return self._hdl._name
        except: return self.explicitFilePath or self.relativeFilePath

    def set( self, **kwArgs ):
        try            : self._log.setLevel( kwArgs['logLevel'] )
        except KeyError: pass
        self._opt.update( kwArgs )


_libPattern = ('libfde*.so*', '*fde.*.dll')[_isWin]
core_loader = LibLoader( fileEnv='LIBFDE', prioPathEnv='FDEPATH', libPattern=_libPattern, matchExisting=True )
