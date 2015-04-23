
__author__      = 'Josef Scheuer'
__versioninfo__ = (0, 0, 1)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = []

def _import( modId, what ):
  _mod = __import__( modId, globals(), locals(), what, -1 )
  for sym in what:
    globals()[sym] = getattr( _mod, sym )
  __all__.extend( what )


_import( 'adt._base',     [] )
_import( 'adt._ftypes',   ['Complex8', 'Complex16', 'Complex32', 'MemoryRef', 'CALLBACK'] )
_import( 'adt._typeinfo', ['TypeInfo'] )
_import( 'adt._string',   ['String'] )
_import( 'adt._item',     ['Item'] )
_import( 'adt._ref',      ['Ref'] )
_import( 'adt._list',     ['List', 'ListIndex'] )
_import( 'adt._hashmap',  ['HashMap', 'HashMapIndex'] )

del _import

