
from .         import Simulator
from traceback import format_exception
import sys


######################################
class RemoteSimulator(Simulator):
######################################

  def extractContext( self, keys, keySep=str ):
    if keySep is str: op = lambda k: [k]
    else            : op = lambda k: k.split(keySep)

    st = self.state
    for k in keys:
      try            : yield (k, st.resolve( op(k) ))
      except KeyError: pass


  def getContext( self, keyList, keySep = str ):
    return dict(self.extractContext( keyList, keySep ))


  def setContext( self, valDict ):
    st = self.state
    for k, v in valDict.items():
      st[k] = v
    return True


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

