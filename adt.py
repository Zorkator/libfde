
from ctypes import *
import sys


cfg  = ('debug', 'release')[0]
arch = (32, 64)[sys.maxsize > 2**32]
tag  = (cfg, arch)

for f in ('libadt.gfortran.{0}.{1}.so', 'libadt.ifort.{0}.{1}.so', 'libadt_dll.dll'):
  soName = f.format(*tag)
  try   : _libHandle = CDLL( soName ); break
  except: print "tried to load '%s' without success ..." % soName
else:
  raise IOError("unable to locate ADT's shared library")



class Meta(type(Structure)):

  def __new__( _class, name, bases, members ):
    from operator import add
    method = '{0}_{{0}}_c_'.format(name.lower())
    members['__typeprocs__'] = list( members.get( '__typeprocs__', [method] ) ) \
                             + reduce( add, (getattr( b, '__typeprocs__', [] ) for b in bases) )
    size = getattr( _libHandle, method.format('object_size'), None )
    size and members.setdefault( '_fields_', [('_data', c_int8 * size())] )
    return super(Meta, _class).__new__( _class, name, bases, members )



class Object(Structure):
  __metaclass__ = Meta
  __typeprocs__ = [] #< no procedures for Object

  def __getattr__( self, name ):
    for fmt in self.__typeprocs__:
      try   : attr = getattr( _libHandle, fmt.format(name) ); break
      except: pass
    else:
      raise AttributeError( "'%s' object has no attribute '%s'" % (self.__class__.__name__, name) )
    setattr( type(self), name, attr )
    return attr



class MemoryRef(Object):
  _fields_ = [('ptr', c_char_p),
              ('len', c_size_t)]



class BaseString(Object):
  _attribute_volatile  = c_int8(0)
  _attribute_permanent = c_int8(1)



class String(BaseString):
  def __init__( self, s = '' ):
    self.init_by_charstring( byref(self), byref(self._attribute_permanent), c_char_p(s), len(s) )
  
  def __str__( self ):
    m = MemoryRef()
    self.memoryref( byref(m), byref(self) )
    return string_at( m.ptr, m.len )

  def __len__( self ):
    return self.len( byref(self) )



class TypeInfo(Object):
  pass



class Ref(Object):
  pass



class Item(Object):
  pass



class List(Object):
  pass


class ListIndex(Object):
  pass



class HashMap(Object):
  pass


