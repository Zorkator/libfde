
from ctypes import CDLL, Union, sizeof, Structure, c_int8
from os     import environ as _env, pathsep as _pathDelim, path as _path

import sys, platform
from distutils.sysconfig import get_python_lib

_archBit     = (32, 64)[sys.maxsize > 2**32]
_archWin     = ('Win32', 'x64')[sys.maxsize > 2**32]
_isWin       = platform.system() == "Windows"
_envBinVar   = ('LD_LIBRARY_PATH', 'PATH')[_isWin]
_libNames    = (
  """
  libadt.debug.{arch}.gfortran.so
  libadt.release.{arch}.gfortran.so
  libadt.debug.{arch}.ifort.so
  libadt.release.{arch}.ifort.so
  libadt.1.gfortran.debug.{arch}.so
  libadt.1.gfortran.release.{arch}.so
  libadt.1.ifort.debug.{arch}.so
  libadt.1.ifort.release.{arch}.so
  """.format( arch = _archBit ),
  """
  libadt.1.Debug.{arch}.dll
  libadt.1.Release.{arch}.dll
  """.format( arch = _archWin )
)[_isWin].split()



class LibLoader(object):

  class Success(Exception):
    pass

  def splitEnvPaths( self, envVarId ):
    return _env.get( envVarId, '' ).split( _pathDelim )


  def _iter_searchPaths( self ):
    pathList = ['.',
                get_python_lib()
               ]
    pathList.extend( self.splitEnvPaths(_envBinVar) )
    return iter(pathList)


  def loadDLL( self, fileName, **kwArgs ):
    envPaths = _env[_envBinVar]
    paths    = [_path.dirname(fileName)] + self.splitEnvPaths( kwArgs.get('prioPathEnv', '') )
    paths.append( _env[_envBinVar] )

    _env[_envBinVar] = _pathDelim.join( paths )
    self.hdl = CDLL( fileName )
    _env[_envBinVar] = envPaths
    self.filePath = fileName
    raise self.Success


  def __init__( self, *libNames, **kwArgs ):
    try:
      # try to get absolute dll-filePath by either kwArgument "filePath" or by given environment variable ...
      fp = kwArgs.get('filePath') or _env.get( kwArgs.get('fileEnv', '') )
      if fp:
        # dll to load was set either directly or via environment variable ...
        self.loadDLL( fp, **kwArgs )
      else:
        # scan search paths for dll to load ...
        for path in self._iter_searchPaths():
          for lib in libNames:
            fp = path + _path.sep + lib
            if _path.isfile( fp ):
              self.loadDLL( fp, **kwArgs )
        else:
          raise OSError( "unable to locate shared library {0}".format(fp) )

    except self.Success:
      sys.stdout.write( "loaded shared library {0}\n".format(fp) )


_libLoader = LibLoader( *_libNames, fileEnv = 'LIBADT', prioPathEnv = 'ADTPATH' )


class _Meta(type(Union)):

  def __new__( _class, name, bases, members ):
    from operator import add
    method = '{0}_{{0}}c_'.format(name.lower())

    # collect list of __typeprocs__ from base classes ...
    members['__typeprocs__'] = list( members.get( '__typeprocs__', [method] ) ) \
                             + reduce( add, (getattr(b, '__typeprocs__', []) for b in bases) )

    # 'inherit' fields of base classes by reserving data space...
    fields = list( members.pop('_fields_', []) ) \
           + list( ('_data.' + b.__name__, b._data.size * c_int8) for b in bases if hasattr(b, '_data') )
    anonym = list( members.pop('_anonymous_', []) )
    size   = getattr( _libLoader.hdl, method.format('object_size_'), lambda: 0 )()

    if fields:
      _Struct = type( '_Struct', (Structure,), dict(_fields_=fields, _anonymous_=anonym) )
      size    = max(size, sizeof(_Struct))
      fields  = [('_struct', _Struct)]
      anonym  = ['_struct']

    size and fields.append( ('_data', size * c_int8) )
    members.update( _fields_=fields, _anonymous_=anonym )
    return super(_Meta, _class).__new__( _class, name, bases, members )



class Compound(Union):
  __metaclass__ = _Meta
  __typeprocs__ = [] #< no procedures for Compound

  @classmethod
  def __getattr__( _class, name ):
    if name is '_needs_delete': #< if we end up here, slot _needs_delete has not been set!
      return False
    if name in ('__members__', '__methods__'):
      return {}

    for fmt in _class.__typeprocs__:
      try   : attr = getattr(_libLoader.hdl, fmt.format(name)); break
      except: pass
    else:
      raise AttributeError("'%s' object has no attribute '%s'" % (_class.__name__, name))
    setattr(_class, name, attr)
    return attr

