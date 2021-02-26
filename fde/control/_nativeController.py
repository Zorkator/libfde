
import os
from ..tools   import OptionProcessor, LibLoader, Wallet, debug
from functools import wraps

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
    return property( _wrapper )


#--------------------------------------------
class NativeController( OptionProcessor ):
#--------------------------------------------
    """Control class for basic handling of C-compatible native code libraries.

    NativeController offers only rudimentary library handling, like lazy loading and customization by options.
    However, is prepared to be extended by inheritance and mixin-classes, what allows composing control features
      according to the native code capabilities.
    """
    __conv__ = dict( lib    = OptionProcessor.realpath )
    __opts__ = dict( lib    = LookupError( 'missing library specification!' )
                   , libEnv = 'FDEPATH'
                   )

    @cached_property
    def about( self ):
        """return dictionary of instance information."""
        return self._get_about()

    # keep this routine overridable!
    def _get_about( self ):
        return dict( pid     = os.getpid()
                   , id      = self._id
                   , classId = type(self).__name__ )


    @cached_property
    def handle( self ):
        """lazy-load property returning handle of loaded library."""
        opts = {'--debug': self._debug > 2, '--verbose': self._verbosity > 2}
        return LibLoader( filePath=self._lib, prioPathEnv=self._libEnv, **opts ).handle


    def __init__( self, **kwArgs ):
        super(NativeController, self).__init__( **kwArgs )
        self._stock    = Wallet()
        self._cloneCnt = 0
        self._id       = 0
        self.initialize()


    def __getstate__( self ):
        self._cloneCnt += 1
        clone = self.__dict__.copy()
        clone['_stock']    = Wallet()
        clone['_cloneCnt'] = 0
        clone['_id']       = self._cloneCnt
        if self._debug > 0: debug()
        return clone


    def __setstate__( self, d ):
        self.__dict__.update( d )
        if self._debug > 0: debug()
        self.initialize()


    def initialize( self, **kwArgs ):
        """triggers initialization of FDEController instance.
        This method is intended to be reimplemented by subclasses and is called internally.
        Usually there's no need to call this explicitly.
        """
        pass
