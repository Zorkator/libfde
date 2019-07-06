
import sys, traceback, threading
from ctypes import c_int32, c_char_p, c_size_t, byref

def threadID():
  return threading.currentThread().ident

######################################
class ExceptionRouter(object):
######################################
  """Mixin class extending NativeController types.

  ExceptionRouter tries to forward python exceptions to controlled native code.
  The native code must provide an C-compatible function for throwing exceptions, with the following signature:

  void throw_c_( int32_t *typeCode, const char *message, size_t length )

  The explicit call to this function is wrapped by the method
    __except__( self, _type, _value, _traceback )
  So in case the native code provides such function with another signature this method could be
    reimplemented to handle this.

  """

  __opts__ = dict( throwFunc = 'throw_c_' )


  ##########################
  class _Hooker(object):
  ##########################
    #################################
    class _ExceptDispatch(object):
    #################################
      _instance = None

      def __new__( _class, *args, **kwArgs ):
        self = _class._instance
        if not self:
          self = _class._instance = object.__new__( _class, *args, **kwArgs )
          self._hooks    = dict()
          self._stdhook  = sys.excepthook
          sys.excepthook = self
        return self


      def __call__( self, *args, **kwArgs ):
        stack = self._hooks.get( threadID(), [] )
        if stack: stack[-1]( *args, **kwArgs )
        else    : self._stdhook( *args, **kwArgs )


      def push( self, hook ):
        stack = self._hooks.setdefault( threadID(), [] )
        stack.append( hook )


      def pop( self ):
        tid   = threadID()
        stack = self._hooks[tid]
        if len(stack) > 1: stack.pop(-1)
        else             : self._hooks.pop( tid )


    def __init__( self, hook ):
      self._hook = hook

    def __enter__( self ):
      if self._hook:
        self._ExceptDispatch().push( self._hook )
      return self

    def __exit__( self, *args ):
      if self._hook:
        self._ExceptDispatch().pop()



  def __except__( self, _type, _value, _traceback ):
    code = c_int32(int('0x02200000', 16)) #< TODO: should map _type to exception code!
    what = ''.join( traceback.format_exception( _type, _value, _traceback ) )
    self.handle[ self._throwFunc ]( byref(code), c_char_p(what), c_size_t(len(what)) )


  def routedExceptions( self ):
    """return with-context that changes Python's excepthook to route Python-Exceptions to native code.
    Leaving the context restores the excepthook set formerly.
    """
    throw = self.handle[ self._throwFunc, None ]
    return self._Hooker( throw and self.__except__ )

