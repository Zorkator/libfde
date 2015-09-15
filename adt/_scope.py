
from _hashmap import HashMap
from _ref     import Ref
from _ftypes  import mappedType, CALLBACK
from ctypes   import byref, c_char_p, c_int


_cbITF_buffer = dict()

@mappedType( 'hashmap', 'type(HashMap_t)' )
class Scope(HashMap):

  def __getitem__( self, ident ):
    valRef = super(Scope, self).__getitem__( ident ).typed
    try   : return valRef.contents
    except: return valRef

  
  def setCallback( self, ident, cbFunc ):
    if type(type(cbFunc)) is not type(CALLBACK):
      _cbITF_buffer['{0}.{1}'.format(id(self), ident)] = func = CALLBACK(cbFunc or 0)
    return self.set_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )

