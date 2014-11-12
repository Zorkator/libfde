
from ctypes import *
import sys

_libHandle = None
_libNames  = 'libadt.{f90c}.{cfg}.{arch}.so, libadt.{cfg}.{arch}.so, libadt.{arch}.so, libadt_dll.dll'
_libInfo   = dict( cfg  = ('debug', 'release')[0],
                   arch = (32, 64)[sys.maxsize > 2**32],
                   f90c = ('gfortran', 'ifort')[0]
                 )

for f in map( str.strip, _libNames.split(',') ):
  _soName = f.format( **_libInfo )
  try:
    _libHandle = CDLL( _soName ); break
  except Exception as e:
    print 'tried loading "%s": %s' % (_soName, e)
else:
  raise OSError("unable to locate ADT's shared library")

print 'loaded "%s"' % _soName



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



class Object(Union):
  __metaclass__ = _Meta
  __typeprocs__ = [] #< no procedures for Object

  def __getattr__( self, name ):
    for fmt in self.__typeprocs__:
      try   : attr = getattr( _libHandle, fmt.format(name) ); break
      except: pass
    else:
      raise AttributeError( "'%s' object has no attribute '%s'" % (self.__class__.__name__, name) )
    setattr( type(self), name, attr )
    return attr



class DynamicObject(Object):
  __slots__ = ['_isRef']

  @property
  def asRef( self ):
    self._isRef = True
    return self

  def __del__( self ):
    getattr( self, '_isRef', False ) or self.delete_( byref(self) )

  def delete( self ):
    self.delete_( byref(self) )

