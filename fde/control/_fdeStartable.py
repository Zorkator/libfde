
from ._startable import Startable
from ctypes      import c_int32, c_char_p, c_size_t, byref

#-------------------------------------
class FDEStartable( Startable ):
#-------------------------------------
    __opts__ = dict( startFunc    = 'start_c_'
                   , finalizeFunc = 'finalize_c_'
                   )


    def __start__( self, *args, **kwArgs ):
        retCode = c_int32()
        self.opts.args = ' '.join( map( str, args ) )
        cmdStr         = self.opts.args.format( **self.about ).encode()
        self.handle[ self.opts.startFunc ]( byref(retCode), c_char_p(cmdStr), c_size_t(len(cmdStr)) )
        return retCode.value


    def __finalize__( self, code, **kwArgs ):
        self.handle[ self.opts.finalizeFunc, lambda: None ]()
        return code
