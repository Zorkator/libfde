
__author__      = 'Josef Scheuer'
__versioninfo__ = (0, 0, 2)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = []

def _import( modId, what ):
  _mod = __import__( modId, globals(), locals(), what, -1 )
  for sym in what:
    globals()[sym] = getattr( _mod, sym )
  __all__.extend( what )


_import( '_ftypes',    ['Complex8', 'Complex16', 'Complex32', 'MemoryRef', 'CALLBACK'] )
_import( '_typeinfo',  ['TypeInfo'] )
_import( '_string',    ['String',  'StringPtr'] )
_import( '_item',      ['Item',    'ItemPtr'] )
_import( '_ref',       ['Ref',     'RefPtr'] )
_import( '_list',      ['List',    'ListPtr'] )
_import( '_hashmap',   ['HashMap', 'HashMapPtr'] )
_import( '_scope',     ['Scope',   'ScopePtr'] )

del _import

