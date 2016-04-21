
from ctypes import CDLL
from os     import environ as _env, pathsep as _pathDelim, path as _path
from glob   import glob
from sys    import stdout
import platform

_isWin = platform.system() == "Windows"
_PATH  = ('LD_LIBRARY_PATH', 'PATH')[_isWin]


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
  def handle( self ):
    try   : return self._hdl
    except:
      if self.opt('debug'):
        import pdb; pdb.set_trace()

      try:
        searchPaths = self.splitEnvPaths( self.opt('prioPathEnv') )
        filePath    = self.opt('filePath') or _env.get( self.opt('fileEnv') ) #< try filePath or environment variable
        if filePath:
          self._tryLoad( filePath, searchPaths )
        else:
          # not found or filePath not given ... so try via libPattern and search paths
          filePath = self.opt('libPattern')
          for path in self.searchpathIter():
            self._tryLoad( path + _path.sep + filePath, searchPaths )

        raise OSError( "unable to load shared library {0}".format(filePath) )

      except self.Success:
        if self.opt('verbose'):
          stdout.write( "loaded shared library {0}\n".format(self._hdl._name) )
      return self._hdl



  def _tryLoad( self, libPattern, searchPaths ):
    envPaths = _env[_PATH]
    paths    = filter( _path.isdir, glob( _path.dirname(libPattern) ) ) + searchPaths
    paths.append( _env[_PATH] )

    _env[_PATH] = _pathDelim.join( paths )
    self._hdl        = None
    for f in glob( libPattern ):
      try   : self._hdl = CDLL( f ); break #< break if load succeeded
      except: pass
    _env[_PATH] = envPaths
    
    if self._hdl:
      raise self.Success
    

  def __init__( self, **kwArgs ):
    self._opt = kwArgs


  def set( self, **kwArgs ):
    self._opt.update( kwArgs )



_libPattern  = ('libadt.*.so', 'libadt.*.dll')[_isWin]
adt_loader = LibLoader( fileEnv='LIBADT', prioPathEnv='ADTPATH', libPattern=_libPattern )

