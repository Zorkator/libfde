
from ctypes      import c_int32, c_char_p, c_size_t, byref
from ._startable import Startable


#-------------------------------------
class FDEStartable( Startable ):
#-------------------------------------
    __opts__ = dict( startFunc    = 'start_c_'
                   , finalizeFunc = 'finalize_c_'
                   )
    __info__ = dict( startFunc    = 'function in `lib` called when starting'
                   , finalizeFunc = 'function in `lib` called after `startFunc` has finished'
                   )


    def __start__( self, *args, **kwArgs ):
        retCode = c_int32()
        self.opts.args = ' '.join( map( str, args ) )
        cmdStr         = self.opts.args.format( **self.about ).encode()
        self.handle[ self.opts.startFunc ]( byref(retCode), c_char_p(cmdStr), c_size_t(len(cmdStr)) )
        return retCode.value


    def __finalize__( self, code, unload = False, **kwArgs ):
        self.handle[ self.opts.finalizeFunc, lambda: None ]()
        if unload:
            del self.handle
        return code
