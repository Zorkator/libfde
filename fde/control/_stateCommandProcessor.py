
from ._baseCommandProcessor import BaseCommandProcessor
from ._stateful             import Stateful


@Stateful.mixin
#------------------------------------------------------
class StateCommandProcessor( BaseCommandProcessor ):
#------------------------------------------------------
    """Mixin class that provides a command loop for processing commands on Stateful ActionContextHosts.
    """

    def _dispatch( self, obj ):
        if   hasattr( obj, 'keys' )    : return self.setData( obj )
        elif hasattr( obj, '__iter__' ): return self.getData( obj )
        else                           : return super(StateCommandProcessor, self)._dispatch( obj )
