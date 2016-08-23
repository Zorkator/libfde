
from traceback import format_exception
import sys, os


######################################
class CommandProcessor(object):
######################################
  """Abstract Mixin class extending Startable, Stateful, Hookable ADTController types.
  
  CommandProcessor provides a simple interface for executing the main loop of Startable codes
    controlled by commands.

  """

  def __init__( self, *args, **kwArgs ):
    super(CommandProcessor, self).__init__( *args, **kwArgs )
    self._doProcess = True


  def processCommands( self ):
    while self._doProcess:
      try:
        res = None
        cmd = self.receive()
        if self._debug > 0:
          import pdb; pdb.set_trace()

        if   isinstance( cmd, basestring ): res = self._dispatchCommand( cmd )
        elif hasattr( cmd, 'keys' )       : res = self.setStateData( cmd )
        elif hasattr( cmd, '__iter__' )   : res = self.getStateData( cmd )
        else                              : res = "unknown command"

      except StopIteration:
        res = 'ok'
        break

      except Exception as e:
        res = e.__class__( ''.join( format_exception( *sys.exc_info() ) ) )

      finally:
        self.send( res )


  def _dispatchCommand( self, cmd ):
    cmd = cmd.split(' ')
    res = getattr( self, "cmd_" + cmd[0] )( *cmd[1:] )
    if res is None: raise StopIteration
    else          : return res


  ####
  # command implementations
  #

  def cmd_setKeySep( self, sep = None ):
    self.setScopeKeySeparator( sep )
    return True

  # Some default commands for Simulators.
  # Subclasses might need to reimplement these.

  def cmd_idle( self )     : return True
  def cmd_state( self )    : return self.state
  def cmd_fork( self )     : return self.fork()
  def cmd_getcwd( self )   : return os.getcwd()

  def cmd_tick( self )     : pass
  def cmd_finalize( self ) : self._doProcess = False
  def cmd_terminate( self ): self._doProcess = False

  def cmd_debug( self, stat ):
    self._debug = int( stat.lower() in 'on true 1 yes enabled'.split() )
    return True
    

  # methods to be [re-]implemented by subclasses

  def receive( self ):
    raise NotImplementedError

  def send( self, what ):
    raise NotImplementedError

