
from ctypes import CDLL, Union, sizeof, Structure, c_int8
from os     import environ as _env, pathsep as _pathDelim, path as _path
from glob   import glob

import sys, platform
from distutils.sysconfig import get_python_lib

_archBit     = (32, 64)[sys.maxsize > 2**32]
_archWin     = ('Win32', 'x64')[sys.maxsize > 2**32]
_isWin       = platform.system() == "Windows"
_envBinVar   = ('LD_LIBRARY_PATH', 'PATH')[_isWin]
_libPattern  = ('libadt.*.so', 'libadt.*.dll')[_isWin]


class LibLoader(object):

  class Success(Exception):
    pass

  @property
  def filePath( self ):
    return getattr( self._hdl, '_name', '' )


  @property
  def handle( self ):
    return self._hdl


  def splitEnvPaths( self, envVarId ):
    return _env.get( envVarId, '' ).split( _pathDelim )


  def _iter_searchPaths( self ):
    pathList = ['.',
                get_python_lib()
               ]
    pathList.extend( self.splitEnvPaths(_envBinVar) )
    return iter(pathList)


  def _loadLib( self, libPattern, pathEnvVar ):
    envPaths = _env[_envBinVar]
    paths    = filter( _path.isdir, glob( _path.dirname(libPattern) ) ) \
             + self.splitEnvPaths( pathEnvVar )
    paths.append( _env[_envBinVar] )

    _env[_envBinVar] = _pathDelim.join( paths )
    self._hdl        = None
    for f in glob( libPattern ):
      try   : self._hdl = CDLL( f ); break #< break if load succeeded
      except: pass
    _env[_envBinVar] = envPaths
    
    if self._hdl:
      raise self.Success
    

  def __init__( self, **kwArgs ):
    self._hdl  = None
    fileEnvVar = kwArgs.get('fileEnv', '')
    pathEnvVar = kwArgs.get('prioPathEnv', '')

    try:
      # try using given filePath or environment variable
      fp = kwArgs.get('filePath') or _env.get( fileEnvVar )
      if fp:
        self._loadLib( fp, pathEnvVar )

      # not found until here ... so try via libPattern and search paths
      fp = kwArgs['libPattern'] #< argument libPattern is NOT optional here!
      for path in self._iter_searchPaths():
        self._loadLib( path + _path.sep + fp, pathEnvVar )

      raise OSError( "unable to locate shared library {0}".format(fp) )

    except self.Success:
      sys.stdout.write( "loaded shared library {0}\n".format(self.filePath) )


_libLoader = LibLoader( fileEnv = 'LIBADT', prioPathEnv = 'ADTPATH', libPattern = _libPattern )


class _Meta(type(Union)):

  def __new__( _class, name, bases, members ):
    from operator    import add
    #from collections import OrderedDict

    def _mro( ident, default ):
      scopes = (members,) + tuple( b.__dict__ for b in bases )
      return filter( None, (s.get( ident ) for s in scopes) ) + [default]

    typeName = _mro( '__typename__', name )[0]
    method   = '{0}_{{0}}c_'.format(typeName.lower())

    # collect list of __typeprocs__ from members and base classes ...
    members.setdefault( '__typeprocs__', [method] )
    procList = reduce( add, _mro( '__typeprocs__', [] ) )
    members['__typeprocs__'] = procList #list( OrderedDict.fromkeys( procList ) )

    # 'inherit' fields of base classes by reserving data space...
    fields = list( members.pop('_fields_', []) ) \
           + list( ('_data.' + b.__name__, b._data.size * c_int8) for b in bases if hasattr(b, '_data') )
    anonym = list( members.pop('_anonymous_', []) )
    size   = getattr( _libLoader.handle, method.format('object_size_'), lambda: 0 )()

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
      try   : attr = getattr(_libLoader.handle, fmt.format(name)); break
      except: pass
    else:
      raise AttributeError("'%s' object has no attribute '%s'" % (_class.__name__, name))
    setattr(_class, name, attr)
    return attr

