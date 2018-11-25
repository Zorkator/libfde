
from . import Wallet

def dict2obj( d ):
  if   hasattr( d, 'keys' )    : return Wallet( (k, dict2obj(v)) for k,v in d.iteritems() )
  elif hasattr( d, '__iter__' ): return map( dict2obj, d )
  else                         : return d

