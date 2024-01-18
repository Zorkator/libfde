
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

    def do_commands( self ):
        tick, self._ticks[:] = self._ticks[:1], self._ticks[1:]
        if tick:
            self._loop = False
            # ... no pending ticks ...
        else:
            super(CommandProcessor, self).do_commands()

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd loop `n` times to continue execution of Startable."""
        self._ticks.extend( (1,) * n )
