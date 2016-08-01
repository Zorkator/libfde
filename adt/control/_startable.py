
import os
from ctypes  import c_int32, c_char_p, c_size_t, byref
from ..tools import makedirs, NullGuard


########################################
class Startable(object):
########################################
  """Mixin class extending ADTController types.
    
  Startable provides the interface for starting the main process loop of native code.
  The main loop is expected to be implemented as:

  void start_c_( int32_t *returnCode, const char *commandString, size_t length )

  This call signature can be changed by overriding the __start__ method.
  After the main loop's exit, an optional finalize routine might be triggered, e.g. for cleanup.
  If such routine exists, its signature is assumed as:
  
  void finalize_c_( void )

  Again, this signature can be changed by overriding the __finalize__ method.

  Furthermore, by the method fork() Startable basically prepares re-starting the native code
    within another Python process.

  """

  __opts__ = dict( args         = ''
                 , startFunc    = 'start_c_'
                 , finalizeFunc = 'finalize_c_'
                 , workdir      = ''
                 )


  def fork( self, **kwArgs ):
    """fork the current Python process and run start() method of instance."""
    from multiprocessing import Process
    childProc = Process( target=self._start, args=(kwArgs,) )
    childProc.start()
    return childProc 


  def _start( self, kwArgs=dict() ):
    self.start( **kwArgs )
  

  def start( self, **kwArgs ):
    # create and change to working directory of simulation ...
    workdir = self._workdir.format( **self.about )
    prevdir = os.getcwd()
    if workdir:
      makedirs( workdir )
      os.chdir( workdir )
    
    # determine argument list ... if not given explicitly use predefined
    args = kwArgs.get( 'args' ) or self._args.format( **self.about )
    try   : args = args.strip and [args] #< if args is string wrap it by list
    except: pass

    with getattr( self, 'routedExceptions', NullGuard )(): #< mixin-method might not be available.
      code = self.__start__( *args, **kwArgs )
      code = self.__finalize__( code, **kwArgs )
    os.chdir( prevdir )
    return code
    

  def __start__( self, *args, **kwArgs ):
    retCode = c_int32()
    cmdStr  = ' '.join( map( str, args ) )
    self.handle[ self._startFunc ]( byref(retCode), c_char_p(cmdStr), c_size_t(len(cmdStr)) )
    return retCode.value


  def __finalize__( self, code, **kwArgs ):
    self.handle[ self._finalizeFunc, lambda: None ]()
    return code
    
