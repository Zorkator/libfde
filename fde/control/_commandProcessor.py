
from ._baseCommandProcessor import BaseCommandProcessor

#-------------------------------------------------
class CommandProcessor( BaseCommandProcessor ):
#-------------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    CommandProcessor extends BaseCommandProcessor by ticks.
    """

    def __init__( self, *args, **kwArgs ):
        super(CommandProcessor, self).__init__( *args, **kwArgs )
        self._ticks = []

    def _processCommand( self ):
        # on pending tick, return StopIteration to end current round of processing commands
        if self._ticks: return self._ticks.pop()
        else          : return super(CommandProcessor, self)._processCommand()

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd processing loop `n` times to continue execution of Startable."""
        self._ticks.extend( (StopIteration,) * n )
