
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 7, 1)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = ['debug']

import logging
logging.basicConfig( level = logging.ERROR )


from ._helper           import NullHandle, Wallet, NullGuard, _arg, auto_raise
from ._libLoader        import LibLoader, core_loader
from ._convert          import dict2obj
from ._files            import sys_channel, openFile, makedirs
from ._optionProcessor  import OptionProcessor

__all__.extend( 'NullHandle Wallet NullGuard LibLoader core_loader dict2obj sys_channel openFile makedirs OptionProcessor'.split() )

class debug(object):
  def __new__( _class ):
    from os import environ as env
    for m in filter( bool, [env.get('ADT_DEBUGGER'), 'pdb'] ):
      try               : start_dbg = __import__( m, globals(), locals() ).set_trace; break
      except ImportError: pass
    return start_dbg()

