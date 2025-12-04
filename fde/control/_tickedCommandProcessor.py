
from ._baseCommandProcessor import BaseCommandProcessor

#-------------------------------------------------------
class TickedCommandProcessor( BaseCommandProcessor ):
#-------------------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    CommandProcessor extends BaseCommandProcessor by ticks.
    """

    def __init__( self, *args, **kwArgs ):
        super(TickedCommandProcessor, self).__init__( *args, **kwArgs )
        self._tickQ = []

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
