
import os
from ..abstract import mixin, abstractmethod
from ..tools    import OptionProcessor, Caching, cached_property, debug


@mixin
#-------------------------------------------------
class Controllable( Caching, OptionProcessor ):
#-------------------------------------------------
    __opts__ = dict( rootId = '{classId}' )
    __info__ = dict( rootId = "root scope id of Controllable type" )

    @cached_property
    def about( self ) -> dict:
        """return dictionary of instance information."""
        return self._get_about()


    # keep this routine overridable!
    def _get_about( self ) -> dict:
        return dict( pid     = os.getpid()
                   , id      = self._id
                   , classId = type(self).__name__
                   , rootId  = self.opts.rootId.format( classId=type(self).__name__ )
                   )


    def __init__( self, **kwArgs ):
        super(Controllable, self).__init__( **kwArgs )
        self._cloneCnt = 0
        self._id       = 0
        self.initialize()


    def __getstate__( self ):
        self._cloneCnt += 1
        clone = super(Controllable, self).__getstate__()
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
    @abstractmethod
    def __initialize__( self, rootId ):
        """Should do initialization of state etc. using rootId."""
        ...
