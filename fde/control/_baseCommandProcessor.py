
from traceback       import format_exception
from ._actionContext import ActionContextHost

#--------------------------------------------------
class BaseCommandProcessor( ActionContextHost ):
#--------------------------------------------------
    """Mixin class extending Startable, Stateful Controller types.
    BaseCommandProcessor provides a simple command loop for executing Stateful
      codes interactively.
    """
    commandPrefix = 'cmd_'
    _prompt       = '>>> '

    def __init__( self, *args, **kwArgs ):
        super(BaseCommandProcessor, self).__init__( *args, **kwArgs )
        self._doProcess = True


    def processCommands( self ):
        while self._doProcess:
            if self.opts.debug > 0:
                from ..tools import debug; debug()
            if self.processCmd() == StopIteration:
                break


    def processCmd( self, cmd = None ):
        try:
            obj = self.receive() if cmd is None else cmd
            if   hasattr( obj, 'strip' )   : res = self.evalCommand( obj )
            elif hasattr( obj, 'keys' )    : res = self.setData( obj )
            elif hasattr( obj, '__iter__' ): res = self.getData( obj )
            else                           : res = "unknown command"
        except Exception as e              : res = e
        #
        if cmd is None: self.send( res )
        else          : return res


    ####
    # command implementations
    #

    def cmd_exit( self ):
        """disable command loop and continue execution of Startable."""
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
