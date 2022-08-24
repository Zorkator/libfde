
from ._stateful import cached_property
from traceback  import format_exception
import os

#--------------------------------------------
class CommandProcessor( object ):
#--------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    CommandProcessor provides a simple command loop for executing Startable and Stateful
      codes interactively.
    """
    _prompt = '>>> '

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
                res = e
            finally:
                self.send( res )


    def evalCommand( self, cmd ):
        ctxt = self.actionContext
        try:
            return ctxt.eval( cmd )
        except SyntaxError:
            ctxt.exec( cmd )


    @cached_property
    def actionContext( self ):
        """return default ActionContext object, using class types for Action, Trigger and VariableLookup."""
        context  = super(CommandProcessor, self).actionContext
        selfType = type(self)
        members  = [ (m[4:], getattr( selfType, m )) for m in dir( selfType ) if m.startswith( 'cmd_' ) ]
        commands = { i: getattr( m, 'fget', m ).__get__(self) for i, m in members } #< treat cmd-properties the same
        context.globals.update( commands )
        return context


    ####
    # command implementations
    #

    def cmd_tick( self, n = 1 ):
        """consecutively exit comamnd loop `n` times to continue execution of Startable."""
        self._ticks.extend( (1,) * n )

    def cmd_exit( self ):
        """disable command loop and continue execution of Startable."""
        self._doProcess = False

    def cmd_globals( self ):
        """return dictionary of global definitions."""
        return self.actionContext.globals

    def cmd_locals( self ):
        """return dictionary of global definitions."""
        return self.actionContext.locals

    # methods that might be reimplemented by subclasses

    def receive( self ):
        try:
            return self.__receive__()
        except EOFError:
            return 'exit()' #< just exit on closed stdin!

    def send( self, what ):
        if what is not None:
            if isinstance( what, Exception ):
                what = ''.join( format_exception( type(what), what, what.__traceback__ ) )
            self.__send__( what )

    def __receive__( self ):
        return input( self._prompt )

    def __send__( self, what ):
        print( what )
