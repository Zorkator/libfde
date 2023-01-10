
import os
from ..tools   import OptionProcessor, Wallet, debug
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
class Controllable( OptionProcessor ):
#--------------------------------------------

    __opts__ = dict( rootId = '{classId}' )

    @cached_property
    def about( self ):
        """return dictionary of instance information."""
        return self._get_about()


    # keep this routine overridable!
    def _get_about( self ):
        return dict( pid     = os.getpid()
                   , id      = self._id
                   , classId = type(self).__name__
                   , rootId  = self.opts.rootId.format( classId = type(self).__name__ )
                   )


    def __init__( self, **kwArgs ):
        super(Controllable, self).__init__( **kwArgs )
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
        if self.opts.debug > 0: debug()
        return clone


    def __setstate__( self, d ):
        self.__dict__.update( d )
        if self.opts.debug > 0: debug()
        self.initialize()


    def initialize( self, **kwArgs ):
        """triggers initialization of Controller instance.
        This is called implicitly by constructor and after deserialization.
        """
        rootId = self.opts.rootId.format( **self.about )
        self.__initialize__( rootId )


    # methods to be [re-]implemented by subclasses

    def __initialize__( self, rootId ):
        """Should do initialization of state etc. using rootId."""
        raise NotImplementedError
