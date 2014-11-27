
from ctypes    import *
from _typeinfo import TypedObject
from _ftypes   import mappedType
from operator  import mul as _mul


@mappedType( 'ref', 'type(Ref_t)' )
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
    rnk = c_size_t(32) #< assume maximum rank of 32
    buf = (c_size_t * rnk.value)()
    self.get_shape_( byref(self), byref(buf), byref(rnk) )
    return buf[:rnk.value]

  @property
  def contentsType( self ):
    ft = self.ftype
    if ft:
      if ft.rank: return reduce( _mul, self.shape, ft.ctype )
      else      : return ft.ctype
    return c_void_p

  @property 
  def contents( self ):
    return cast( self.ptr, POINTER(self.contentsType) ).contents

  def clone( self ):
    other = Ref()
    self.clone_( byref(other), byref(self) )
    return other

