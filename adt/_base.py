
from ctypes import CDLL, Union, sizeof, Structure, c_int8
from os     import environ as _env, pathsep as _pathsep, path as _path
import sys, platform
from distutils.sysconfig import get_python_lib

_archBit     = (32, 64)[sys.maxsize > 2**32]
_archWin     = ('Win32', 'x64')[sys.maxsize > 2**32]
_libHandle   = None
_libFilePath = None
_libNames    = (
  """
	libadt.debug.{arch}.gfortran.so
	libadt.release.{arch}.gfortran.so
	libadt.debug.{arch}.ifort.so
	libadt.release.{arch}.ifort.so
  libadt.0.gfortran.debug.{arch}.so
  libadt.0.gfortran.release.{arch}.so
  libadt.0.ifort.debug.{arch}.so
  libadt.0.ifort.release.{arch}.so
  """.format( arch = _archBit ),
  """
  libadt.0.Debug.{arch}.dll
  libadt.0.Release.{arch}.dll
  """.format( arch = _archWin )
)[platform.system() == "Windows"].split()


def _iter_searchPaths():
  pathList = ['.',
              get_python_lib()
             ]
  pathList.extend( (_env.get('LD_LIBRARY_PATH') or _env['PATH']).split( _pathsep ) )
  return iter(pathList)


class BreakLoops(Exception): pass

try:
  for path in _iter_searchPaths():
    for lib in _libNames:
      _libFilePath = _path.abspath( path ) + _path.sep + lib
      if _path.isfile( _libFilePath ):
        _libHandle = CDLL( _libFilePath )
        raise BreakLoops
  else:
    raise OSError("unable to locate ADT's shared library")
except BreakLoops:
  print "loaded shared library " + _libFilePath



class _Meta(type(Union)):

  def __new__( cls, name, bases, members ):
    from operator import add
    method = '{0}_{{0}}c_'.format(name.lower())
    members['__typeprocs__'] = list( members.get( '__typeprocs__', [method] ) ) \
                             + reduce( add, (getattr(b, '__typeprocs__', []) for b in bases) )

    fields = list( members.pop('_fields_', []) )
    anonym = list( members.pop('_anonymous_', []) )
    size   = getattr( _libHandle, method.format('object_size_'), lambda: 0 )()

    if fields:
      _Struct = type( '_Struct', (Structure,), dict(_fields_=fields, _anonymous_=anonym) )
      size    = max(size, sizeof(_Struct))
      fields  = [('_struct', _Struct)]
      anonym  = ['_struct']

    size and fields.append( ('_data', size * c_int8) )
    members.update( _fields_=fields, _anonymous_=anonym )
    return super(_Meta, cls).__new__( cls, name, bases, members )



class Compound(Union):
  __metaclass__ = _Meta
  __typeprocs__ = [] #< no procedures for Compound

  def __getattr__( self, name ):
    if name is '_needs_delete': #< if we end up here, slot _needs_delete has not been set!
      return False
    if name in ('__members__', '__methods__'):
      return {}

    for fmt in self.__typeprocs__:
      try   : attr = getattr(_libHandle, fmt.format(name)); break
      except: pass
    else:
      raise AttributeError("'%s' object has no attribute '%s'" % (self.__class__.__name__, name))
    setattr(type(self), name, attr)
    return attr

