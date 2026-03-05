
from ._startable import Startable

#-------------------------------------
class PyStartable( Startable ):
#-------------------------------------
    __opts__ = dict( startFunc    = 'start'
                   , finalizeFunc = 'finalize'
                   )
    __info__ = dict( startFunc    = 'method name called when starting'
                   , finalizeFunc = 'method name called after `startFunc` has finished'
                   )


    def __start__( self, *args, **kwArgs ):
        return getattr( self.handle, self.opts.startFunc )( *args, **kwArgs )


    def __finalize__( self, code, **kwArgs ):
        getattr( self.handle, self.opts.finalizeFunc, lambda **kwArgs: None )( **kwArgs )
        return code
