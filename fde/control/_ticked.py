
from ._baseCommandProcessor import BaseCommandProcessor
from ._hookable             import Hookable, connect_to_hook
from ..tools                import OptionProcessor as OP


@BaseCommandProcessor.mixin.requires( Hookable )
#-------------------------------------------------------
class Ticked( object ):
#-------------------------------------------------------
    """Mixin class extending Hookable BaseCommandProcessor types by command processing at
      configurable commandHook[s].
    """
    __conv__ = dict( commandHook=OP.unique(OP.list(str)) )
    __opts__ = dict( commandHook=[] )
    __info__ = dict( commandHook='hook name[s] for which to start command loop' )

    def __init__( self, *args, **kwArgs ):
        super(Ticked, self).__init__( *args, **kwArgs )
        self._tickQ = []

    def initialize( self ):
        connect_to_hook( *self.opts.commandHook )( self.processCommands )
        super(Ticked, self).initialize()

    def processCmd( self, cmd = None ):
        # on pending tick, return a queued _loopExit to end current round of processing commands
        if self._tickQ: return self._tickQ.pop()
        else          : return super(Ticked, self).processCmd( cmd )

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd processing loop `n` times to continue execution of Startable."""
        self._tickQ.extend( (self._loopExit,) * n )
        return "tack"
