
from ._stateful import cached_property
from traceback  import format_exception
import sys, os

#--------------------------------------------
class CommandProcessor( object ):
#--------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    Used interfaces:
      Stateful : setData, getData, makeKeyTokenizer, state
      Startable: fork

    CommandProcessor provides a simple command loop for executing Startable and Stateful
      codes interactively.
    """
    _prompt = '>>> '
    CMD_OK  = 0xC0DE0

    def __init__( self, *args, **kwArgs ):
        super(CommandProcessor, self).__init__( *args, **kwArgs )
        self._doProcess = True
        self._ticks     = []


    def processCommands( self ):
        while self._doProcess:
            tick, self._ticks[:] = self._ticks[:1], self._ticks[1:]
            if tick:
                break
            # ... no pending ticks ...
            try:
                cmd = self.receive()
                if self.opts.debug > 0:
                    from ..tools import debug; debug()
                if   hasattr( cmd, 'strip' )   : res = self.evalCommand( cmd )
                elif hasattr( cmd, 'keys' )    : res = self.setData( cmd )
                elif hasattr( cmd, '__iter__' ): res = self.getData( cmd )
                else                           : res = "unknown command"
            except Exception as e:
                res = e.__class__( ''.join( format_exception( *sys.exc_info() ) ) )
            finally:
                self.send( res )


    def evalCommand( self, cmd ):
        ctxt = self.actionContext
        try:
            return ctxt.eval( cmd )
        except SyntaxError:
            ctxt.exec( cmd )
            return self.CMD_OK


    @cached_property
    def actionContext( self ):
        """return default ActionContext object, using class types for Action, Trigger and VariableLookup."""
        context  = super(CommandProcessor, self).actionContext
        selfType = type(self)
        members  = [ getattr( selfType, m ) for m in dir( selfType ) if m.startswith( 'cmd_' ) ]
        commands = [ getattr( f, 'fget', f ) for f in members ] #< treat cmd-properties the same
        context.globals.update(
            (cmd.__name__[4:], cmd.__get__(self)) for cmd in commands
        )
        return context


    ####
    # command implementations
    #

    def cmd_setKeyOp( self, sep = None ):
        self.keyTokenizer = self.makeKeyTokenizer( sep )

    # Some default commands for Simulators.
    # Subclasses might need to reimplement these.

    def cmd_idle( self )     : return True
    def cmd_state( self )    : return self.state
    def cmd_getcwd( self )   : return os.getcwd()

    def cmd_tick( self, n=1 ): self._ticks.extend( (1,) * n )
    def cmd_finalize( self ) : self._doProcess = False
    def cmd_terminate( self ): self._doProcess = False

    def cmd_fork( self, **kwArgs ):
        return self.fork( **kwArgs )

    def cmd_debug( self, stat ):
        self.opts.debug = int( stat.lower() in 'on true 1 yes enabled'.split() )

    # methods to be [re-]implemented by subclasses

    def receive( self ):
        return input( self._prompt )

    def send( self, what ):
        if what is not self.CMD_OK:
            print( repr(what) )
