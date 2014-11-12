
from ctypes    import *
from _typeinfo import TypedObject
from _ftypes   import fortranType


@fortranType( 'type(Ref_t)' )
class Ref(TypedObject):

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


