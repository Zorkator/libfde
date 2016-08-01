
from traceback import format_exception
import sys, os


######################################
class CommandProcessor(object):
######################################
  """Abstract Mixin class extending Startable, Stateful, Hookable ADTController types.
  
  CommandProcessor provides a simple interface for executing the main loop of Startable codes
    controlled by commands.
  

  """

  def processCommands( self ):
    while True:
      try:
        cmd, res = self.receive(), None
        if   isinstance( cmd, basestring ): res = self._dispatchCommand( cmd.split() )
        elif hasattr( cmd, 'keys' )       : res = self.setStateData( cmd )
        elif hasattr( cmd, '__iter__' )   : res = self.getStateData( cmd )
        else                              : res = "unknown command"

      except StopIteration:
        res = 'ok'
        break

      except Exception:
        res = ''.join( format_exception( *sys.exc_info() ) )

      finally:
        self.send( res )


  def _dispatchCommand( self, cmd ):
    return getattr( self, "cmd_" + cmd[0] )( *cmd[1:] )


  ####
  # command implementations
  #

  def cmd_setKeySep( self, sep = None ):
    self.setScopeKeySeparator( sep )
    return True

  # Some default commands for Simulators.
  # Subclasses might need to reimplement these.

  def cmd_idle( self )     : return True
  def cmd_state( self )    : return self.context
  def cmd_fork( self )     : return self.fork()
  def cmd_tick( self )     : raise StopIteration
  def cmd_terminate( self ): raise StopIteration
  def cmd_getcwd( self )   : return os.getcwd()


  # methods to be [re-]implemented by subclasses

  def receive( self ):
    raise NotImplementedError

  def send( self, what ):
    raise NotImplementedError

