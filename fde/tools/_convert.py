
from . import Wallet

def dict2obj( d ):
    if   hasattr( d, 'keys' )    : return Wallet( (k, dict2obj( v )) for k, v in d.items() )
    elif hasattr( d, '__iter__' ): return list( map( dict2obj, d ) )
    else                         : return d
