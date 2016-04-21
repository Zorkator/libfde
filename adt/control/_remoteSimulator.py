
from .         import Simulator
from traceback import format_exception
import sys


######################################
class RemoteSimulator(Simulator):
######################################

  def processCommands( self ):
    while True:
      try:
        cmd, res = self.receive(), None
        if   isinstance( cmd, basestring ): res = getattr( self, "cmd_%s" % cmd )()
        elif hasattr( cmd, 'keys' )       : res = self.setContext( cmd )
        elif hasattr( cmd, '__iter__' )   : res = self.getContext( cmd )
        else                              : res = "unknown command"

      except StopIteration:
        res = 'ok'
        break

      except Exception:
        res = ''.join( format_exception( *sys.exc_info() ) )

      finally:
        self.send( res )


  # methods to be [re-]implemented by subclasses

  def receive( self ):
    raise NotImplementedError

  def send( self, what ):
    raise NotImplementedError

