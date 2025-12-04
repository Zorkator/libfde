
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 6)
__version__     = '.'.join( map( str, __versioninfo__ ) )

from ._helper           import NullHandle, Wallet, TypeObject, mkTypeObject, NullGuard, _arg, auto_raise, _decorate
from ._libLoader        import LibLoader, core_loader, CDLL_t
from ._convert          import dict2obj
from ._files            import sys_channel, openFile, makedirs
from ._optionProcessor  import OptionProcessor
from ._objectFactory    import ObjectFactory, NamedObjectFactory
from ._weakList         import WeakList
from ._caching          import Caching, cached_property


#-------------------------------------------
class debug( object ):
#-------------------------------------------
    def __new__( _class ):
        from os import environ as env
        for m in filter( bool, [env.get( 'FDE_DEBUGGER' ), 'pdb'] ):
            try               : start_dbg = __import__( m, globals(), locals() ).set_trace; break
            except ImportError: pass
        return start_dbg()
