
from _hashmap import HashMap
from _ref     import Ref
from _ftypes import mappedType


@mappedType( 'hashmap', 'type(HashMap_t)' )
class Scope(HashMap):

  def __getitem__( self, ident ):
    valRef = super(Scope, self).__getitem__( ident ).typed
    try   : return valRef.contents
    except: return valRef

