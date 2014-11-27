
from ctypes import *
import sys, platform

_libHandle  = None
_libArch    = (32, 64)[sys.maxsize > 2**32]
_libArchWin = ('Win32', 'x64')[sys.maxsize > 2**32]
_libNames   = ("""
  libadt.0.gfortran.debug.{arch}.so
  libadt.0.gfortran.release.{arch}.so
  libadt.0.ifort.debug.{arch}.so
  libadt.0.ifort.release.{arch}.so
""",
"""
  libadt.0.Debug.{archWin}.dll
  libadt.0.Release.{archWin}.dll
""")[platform.system() == "Windows"]

for soName in map( str.strip, _libNames.format( arch = _libArch, archWin = _libArchWin ).split() ):
  try:
    _libHandle = CDLL( soName ); break
  except Exception as e:
    print 'tried loading "%s": %s' % (soName, e)
else:
  raise OSError("unable to locate ADT's shared library")

print 'loaded "%s"' % soName



class _Meta(type(Union)):

  def __new__( _class, name, bases, members ):
    from operator import add
    method = '{0}_{{0}}c_'.format(name.lower())
    members['__typeprocs__'] = list( members.get( '__typeprocs__', [method] ) ) \
                             + reduce( add, (getattr( b, '__typeprocs__', [] ) for b in bases) )

    fields = list( members.pop('_fields_', []) )
    anonym = list( members.pop('_anonymous_', []) )
    size   = getattr( _libHandle, method.format('object_size_'), lambda: 0 )()
    
    if fields:
      _Struct = type( '_Struct', (Structure,), dict( _fields_ = fields, _anonymous_ = anonym ) )
      size    = max( size, sizeof(_Struct) )
      fields  = [('_struct', _Struct)]
      anonym  = ['_struct']

    size and fields.append( ('_data', size * c_int8) )
    members.update( _fields_ = fields, _anonymous_ = anonym )
    return super(_Meta, _class).__new__( _class, name, bases, members )



class Compound(Union):
  __metaclass__ = _Meta
  __typeprocs__ = [] #< no procedures for Compound
  __slots__     = ['_is_reference']

  def __getattr__( self, name ):
    if name is '_is_reference': #< if we end up here, slot _is_reference has not been set!
      return False
    if name in ('__members__', '__methods__'):
      return {}

    for fmt in self.__typeprocs__:
      try   : attr = getattr( _libHandle, fmt.format(name) ); break
      except: pass
    else:
      raise AttributeError( "'%s' object has no attribute '%s'" % (self.__class__.__name__, name) )
    setattr( type(self), name, attr )
    return attr

