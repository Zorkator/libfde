
__author__      = 'Josef Scheuer'
__versioninfo__ = (0, 0, 3)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = ['debug']

import logging
logging.basicConfig( level = logging.ERROR )

def _import( modId, what ):
  _mod = __import__( modId, globals(), locals(), what, -1 )
  for sym in what:
    globals()[sym] = getattr( _mod, sym )
  __all__.extend( what )


_import( '_helper',          ['NullHandle', 'Wallet', 'NullGuard'] )
_import( '_libLoader',       ['LibLoader', 'core_loader'] )
_import( '_convert',         ['dict2obj'] )
_import( '_files',           ['sys_channel', 'openFile', 'makedirs'] )
_import( '_optionProcessor', ['OptionProcessor'] )


class debug(object):
  def __new__( _class ):
    from os import environ as env
    for m in filter( bool, [env.get('ADT_DEBUGGER'), 'pdb'] ):
      try               : start_dbg = __import__( m, globals(), locals() ).set_trace; break
      except ImportError: pass
    return start_dbg()

