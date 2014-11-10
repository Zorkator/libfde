
from ctypes import *
import sys, __builtin__

_libNames = getattr( __builtin__, '_adt_libnames', 'libadt.{cmpl}.{cfg}.{arch}.so, libadt.{cfg}.{arch}.so, libadt.{arch}.so libadt_dll.dll')
_libInfo  = dict( cfg  = getattr( __builtin__, '_adt_cfg', 'debug'),
                  arch = (32, 64)[sys.maxsize > 2**32],
                  cmpl = getattr( __builtin__, '_adt_cmpl', 'gfortran'),
                )

for f in map( str.strip, _libNames.split(',') ):
  soName = f.format( **_libInfo )
  try   : _libHandle = CDLL( soName ); break
  except: print "tried to load '%s' without success ..." % soName
else:
  raise IOError("unable to locate ADT's shared library")



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



class _Object(Union):
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



class MemoryRef(Structure):
  _fields_ = [('ptr', c_void_p),
              ('len', c_size_t)]

  def __str__( self ):
    return string_at( self.ptr, self.len )



class TypeSpecs(Structure):
  pass

TypeSpecs._fields_ = [('typeId',   MemoryRef),
                      ('baseType', MemoryRef),
                      ('byteSize', c_size_t),
                      ('rank',     c_size_t),
                      ('subtype',  POINTER(TypeSpecs))]



class _DynamicObject(_Object):

  def __del__( self ):
    self.delete_( byref(self) )

  def delete( self ):
    self.delete_( byref(self) )



class BaseString(_DynamicObject):
  _attribute_volatile  = c_int8(0)
  _attribute_permanent = c_int8(1)



class String(BaseString):
  def __init__( self, s = '' ):
    self.init_by_charstring_( byref(self), byref(self._attribute_permanent), c_char_p(s), len(s) )
  
  def __str__( self ):
    m = MemoryRef()
    self.memoryref_( byref(m), byref(self) )
    return string_at( m.ptr, m.len )

  def __len__( self ):
    return self.len_( byref(self) )

  def assign( self, other ):
    if isinstance( other, String ): self.assign_string_( byref(self), byref(other) )
    else                          : self.assign_



class TypeInfo(_Object):
  _fields_    = [('_spec', TypeSpecs)]
  _anonymous_ = ['_spec']



class _TypedObject(_DynamicObject):
  _fields_ = [('typeInfo', POINTER(TypeInfo))]



class Ref(_TypedObject):

  @property
  def ptr( self ):
    p = POINTER(c_void_p)()
    self.cptr_( byref(p), byref(self) )
    return p

  @property
  def rank( self ):
    return self.rank_( byref(self) )

  @property
  def shape( self ):
    rnk = c_size_t(32)
    buf = (c_size_t * rnk.value)()
    self.get_shape_( byref(self), byref(buf), byref(rnk) )
    return buf[:rnk.value]

  def clone( self ):
    other = Ref()
    self.clone_( byref(other), byref(self) )
    return other

  def assign( self, other ):
    self.assign_( byref(self), byref(other) )



class Item(_TypedObject):
  pass



class List(_DynamicObject):
  pass


class ListIndex(_Object):
  pass



class HashMap(_DynamicObject):
  pass


