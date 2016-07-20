
__author__      = 'Josef Scheuer'
__versioninfo__ = (0, 0, 3)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = []

def _import( modId, what ):
  _mod = __import__( modId, globals(), locals(), what, -1 )
  for sym in what:
    globals()[sym] = getattr( _mod, sym )
  __all__.extend( what )


_import( '_helper',    ['NullHandle', 'Wallet'] )
_import( '_libLoader', ['LibLoader', 'core_loader'] )
_import( '_convert',   ['dict2obj'] )

