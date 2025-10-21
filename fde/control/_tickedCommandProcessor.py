
from ._baseCommandProcessor import BaseCommandProcessor
from ._hookable             import connect_to_hook

#-------------------------------------------------------
class TickedCommandProcessor( BaseCommandProcessor ):
#-------------------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    CommandProcessor extends BaseCommandProcessor by ticks.
    """
    __opts__ = dict( commandHooks='' )
    __conv__ = dict( commandHooks=lambda s: [*filter( bool, (h.strip() for h in s.split( ',' )) )] )

    def __init__( self, *args, **kwArgs ):
        super(TickedCommandProcessor, self).__init__( *args, **kwArgs )
        self._tickQ = []


    def initialize( self ):
        connect_to_hook( *self.opts.commandHooks )( self.processCommands )
        super(BaseCommandProcessor, self).initialize()


    def processCmd( self, cmd = None ):
        # on pending tick, return a queued _loopExit to end current round of processing commands
        if self._tickQ: return self._tickQ.pop()
        else          : return super(TickedCommandProcessor, self).processCmd( cmd )

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd processing loop `n` times to continue execution of Startable."""
        self._tickQ.extend( (self._loopExit,) * n )
        return "tack"


# compatibility
CommandProcessor = TickedCommandProcessor
