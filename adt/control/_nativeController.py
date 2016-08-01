
import os
from ..tools import OptionProcessor, LibLoader, Wallet


##############################################
class NativeController(OptionProcessor):
##############################################
  """Control class for basic handling of C-compatible native code libraries.

  NativeController offers only rudimentary library handling, like lazy loading and customization by options.
  However, is prepared to be extended by inheritance and mixin-classes, what allows composing control features
    according to the native code capabilities.

  """
  _cloneCnt = 0
  __opts__  = dict( lib    = LookupError('missing library specification!')
                  , libEnv = 'ADTPATH'
                  )

  @property
  def about( self ):
    """return dictionary of instance information."""
    try   : return self._stock._about
    except:
      self._stock._about = self._get_about()
      return self._stock._about

  
  def _get_about( self ):
    return dict( pid     = os.getpid()
               , id      = self._cloneCnt
               , classId = type(self).__name__ )


  @property
  def handle( self ):
    """lazy-load property returning handle of loaded library."""
    try   : return self._stock._handle
    except:
      self._stock._handle = LibLoader( filePath=self._lib, prioPathEnv=self._libEnv ).handle
      return self._stock._handle


  def __init__( self, **kwArgs ):
    super(NativeController, self).__init__( **kwArgs )
    self._lib      = os.path.abspath( self._lib )
    self._stock    = Wallet()
    self._cloneCnt = 0
    self.initialize()


  def __getstate__( self ):
    self._cloneCnt += 1
    clone = self.__dict__.copy()
    clone['_stock']    = Wallet()
    clone['_cloneCnt'] = 0
    return clone


  def __setstate__( self, d ):
    self.__dict__.update( d )
    self.initialize()


  def initialize( self, **kwArgs ):
    """triggers initialization of ADTController instance.
    This method is intended to be reimplemented by subclasses and is called internally.
    Usually there's no need to call this explicitly.

    """
    pass


