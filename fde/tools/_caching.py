
from functools import wraps
from ._helper  import Wallet

#--------------------------------------------
class Caching( object ):
#--------------------------------------------
    """Mixin class that allows using cached_property.
    Caching stores the values of such properties in Wallet that gets excluded when pickling!
    """

    def __init__( self, *args, **kwArgs ):
        super( Caching, self ).__init__( *args, **kwArgs )
        self._stock = Wallet()

    def __getstate__( self ):
        state = self.__dict__.copy()
        state['_stock'] = Wallet()
        return state


#----------------------------
def cached_property( f ):
#----------------------------
    @wraps( f )
    def _wrapper( self ):
        try:
            return getattr( self._stock, '_p_' + f.__name__ )
        except AttributeError:
            val = f( self )
            setattr( self._stock, '_p_' + f.__name__, val )
            return val
    #
    return property( _wrapper )
