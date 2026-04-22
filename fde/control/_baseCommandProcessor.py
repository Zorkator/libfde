
from traceback       import format_exception
from ._actionContext import ActionContextHost
from ..abstract      import mixin


@mixin
#--------------------------------------------------
class BaseCommandProcessor( ActionContextHost ):
#--------------------------------------------------
    """Mixin class that provides a command loop for processing commands on ActionContextHosts.
    """
    commandPrefix = 'cmd_'
    _prompt       = '>>> '
    _loopExit     = frozenset()

    def __init__( self, *args, **kwArgs ):
        super(BaseCommandProcessor, self).__init__( *args, **kwArgs )
        self._doProcess = True


    def processCommands( self ):
        while self._doProcess:
            if self.opts.debug > 0:
                from ..tools import debug; debug()
            if self.processCmd() is self._loopExit:
                break


    def processCmd( self, cmd = None ):
        try:
            obj = self.receive() if cmd is None else cmd
            if hasattr( obj, 'strip' ): res = self.evalCommand( obj )
            else                      : res = self._dispatch( obj )
        except StopIteration : res = self.cmd_exit()
        except Exception as e: res = e
        #
        if cmd is None: self.send( res )
        else          : return res


    def _dispatch( self, obj ):
        return LookupError( "unknown command" )

    ####
    # command implementations
    #

    def cmd_exit( self ):
        """disable command loop and continue execution of processCommands-caller."""
        self._doProcess = False
        return 'bye!'

    def cmd_globals( self ):
        """return dictionary of global definitions."""
        return self.actionContext.globals

    def cmd_locals( self ):
        """return dictionary of global definitions."""
        return self.actionContext.locals

    # methods that might be reimplemented by subclasses

    def receive( self ):
        return self.__receive__()

    def send( self, what ):
        self.__send__( what )

    def __receive__( self ):
        try:
            return input( self._prompt )
        except EOFError:
            return 'exit()'  # < just exit on closed stdin!

    def __send__( self, what ):
        if what is not None:
            if isinstance( what, Exception ):
                what = ''.join( format_exception( type(what), what, what.__traceback__ ) )
            print( what )
