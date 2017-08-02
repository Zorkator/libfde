
from ctypes import CDLL as _CDLL
from os     import environ as _env, pathsep as _pathDelim, path as _path, getpid as _getpid
from glob   import glob
from sys    import stdout
from .      import logging
import platform

_isWin = platform.system() == "Windows"
_PATH  = ('LD_LIBRARY_PATH', 'PATH')[_isWin]


######################################
class CDLL_t(_CDLL):
######################################
  def __getitem__( self, ident ):
    """if given more than one argument, try one after another before giving up and returning the last as default."""
    if isinstance( ident, tuple ):
      for i in ident[:-1]:
        try                  : return super(CDLL_t, self).__getitem__( i )
        except AttributeError: pass
      return ident[-1]
    else:
      return super(CDLL_t, self).__getitem__( ident )



######################################
class LibLoader(object):
######################################

  class Success(Exception):
    pass

  @staticmethod
  def splitEnvPaths( envVarId ):
    return _env.get( envVarId, '' ).split( _pathDelim )


  def opt( self, id, default='' ):
    return self._opt.get( id, default )


  def searchpathIter( self ):
    from distutils.sysconfig import get_python_lib

    pathList = ['.']
    pathList.extend( self.splitEnvPaths( self.opt('prioPathEnv') ) )
    pathList.append( get_python_lib() )
    pathList.extend( self.splitEnvPaths( _PATH ) )
    return iter(pathList)


  @property
  def explicitFilePath( self ):
    """return explicit filePath setting, either by argument or environment variable."""
    return self.opt('filePath') or _env.get( self.opt('fileEnv') )


  @property
  def relativeFilePath( self ):
    """return relative filePath setting, applicable to search paths."""
    return self.opt('libPattern')


  @property
  def handle( self ):
    try   : return self._hdl
    except:
      if self.opt('--debug'):
        from . import debug; debug()

      try:
        searchPaths = self.splitEnvPaths( self.opt('prioPathEnv') )
        filePath    = self.explicitFilePath
        if filePath:
          # try loading for explicit setting
          filePath = self._tryMatch( filePath ) #< try matching with already loaded libraries
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

        raise OSError( "unable to load shared library {0}".format(filePath) )

      except self.Success:
        self._log.info( "loaded shared library {0}\n".format(self._hdl._name) )
      return self._hdl



  def _tryLoad( self, libPattern, searchPaths ):
    envPaths = _env[_PATH]
    paths    = list(filter( _path.isdir, glob( _path.dirname(libPattern) ) )) + searchPaths
    paths.append( _env[_PATH] )

    _env[_PATH] = _pathDelim.join( paths )
    self._hdl   = None
    for f in glob( libPattern ):
      self._log.debug( "try loading " + f )
      try   : self._hdl = CDLL_t( str(f) ); break #< break if load succeeded
      except: pass
    _env[_PATH] = envPaths

    if self._hdl:
      raise self.Success


  def _tryMatch( self, libPattern ):
    if libPattern and self.opt( 'matchExisting' ):
      try  : import psutil, fnmatch
      except ImportError:
        self._log.error( "library matching not available, need packages psutil and fnmatch!\n" )
      else:
        prefix = ('*/', '')[_path.isabs(libPattern)]
        p = psutil.Process( _getpid() )
        for lib in p.memory_maps():
          if fnmatch.fnmatch( lib.path, _path.normpath(prefix + libPattern) ):
            libPattern = lib.path
            self._log.info( "matched already loaded library {0}".format(libPattern) )
            break
    return libPattern


  def __init__( self, **kwArgs ):
    self._opt = kwArgs
    self._log = logging.getLogger( type(self).__name__ )


  def __str__( self ):
    try   : return self._hdl._name
    except: return self.explicitFilePath or self.relativeFilePath


  def set( self, **kwArgs ):
    self._opt.update( kwArgs )



_libPattern = ('libadt.*.so', 'libadt.*.dll')[_isWin]
core_loader = LibLoader( fileEnv='LIBADT', prioPathEnv='ADTPATH', libPattern=_libPattern, matchExisting=True )

