
from ._startable import Startable

#-------------------------------------
class PyStartable( Startable ):
#-------------------------------------
    __opts__ = dict( startFunc    = 'start'
                   , finalizeFunc = 'finalize'
                   )


    def __start__( self, *args, **kwArgs ):
        return getattr( self.handle, self.opts.startFunc )( *args, **kwArgs )


    def __finalize__( self, code, **kwArgs ):
        getattr( self.handle, self.opts.finalizeFunc, lambda **kwArgs: None )( **kwArgs )
        return code
