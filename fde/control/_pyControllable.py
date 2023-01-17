
from ._controllable import Controllable
from ..tools        import cached_property

#---------------------------------------
class PyControllable( Controllable ):
#---------------------------------------
    __conv__ = dict( simClass = Controllable.import_def )
    __opts__ = dict( simClass = LookupError( 'missing class specification for python simulator!' )
                   , initFunc = '' #< additional initialization (in addition to constructor)
                   )

    @cached_property
    def handle( self ):
        """lazy-load property returning handle to controllable object
        The controller options get forwarded to the created controllable object.
        """
        opts = dict( zip( self.opts, self.opts[self.opts] ) ) #< make option
        return self.opts.simClass( **dict( opts, **self.about ) )


    def __initialize__( self, rootId ):
        """ensure handle creation and, if needed, call additional initFunc."""
        hdl, init = self.handle, self.opts.initFunc
        if init:
            getattr( hdl, init )( rootId )
