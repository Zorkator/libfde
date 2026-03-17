
from ._baseCommandProcessor import BaseCommandProcessor
from ._hookable             import Hookable, connect_to_hook
from ..tools                import OptionProcessor


@BaseCommandProcessor.mixin.requires( Hookable )
#-------------------------------------------------------
class Ticked( object ):
#-------------------------------------------------------
    """Mixin class extending Hookable BaseCommandProcessor types by command processing at
      configurable commandHooks.
    """
    __conv__ = dict( commandHooks=OptionProcessor.list(str) )
    __opts__ = dict( commandHooks=[] )
    __info__ = dict( commandHooks='hook names for which to start command loop' )

    def __init__( self, *args, **kwArgs ):
        super(Ticked, self).__init__( *args, **kwArgs )
        self._tickQ = []

    def initialize( self ):
        connect_to_hook( *self.opts.commandHooks )( self.processCommands )
        super(Ticked, self).initialize()

    def processCmd( self, cmd = None ):
        # on pending tick, return a queued _loopExit to end current round of processing commands
        if self._tickQ: return self._tickQ.pop()
        else          : return super(Ticked, self).processCmd( cmd )

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd processing loop `n` times to continue execution of Startable."""
        self._tickQ.extend( (self._loopExit,) * n )
        return "tack"
