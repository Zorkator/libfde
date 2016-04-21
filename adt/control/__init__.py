
__author__      = 'Josef Scheuer'
__versioninfo__ = (0, 0, 1)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = []

def _import( modId, what ):
  _mod = __import__( modId, globals(), locals(), what, -1 )
  for sym in what:
    globals()[sym] = getattr( _mod, sym )
  __all__.extend( what )


_import( '_libLoader',       ['LibLoader', 'adt_loader'] )
_import( '_simulator',       ['Simulator'] )
_import( '_remoteSimulator', ['RemoteSimulator'] )

del _import
