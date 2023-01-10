
from ..core    import Scope
from ..control import Variable
from ..tools   import mkTypeObject, OptionProcessor

#-----------------------------------
class Simulator(OptionProcessor):
#-----------------------------------
    _rootId       = '{!r}'
    _stateLocator = '{._rootId}/state'
    _hooksLocator = '{._rootId}/hooks'
    __state__     = dict( doTerminate = False )
    __hooks__     = dict( initialization_done = None )
    __defaults__  = dict()

    def _get_path_scope( self, locator ):
        "get or create scope stored in process, located at /-separated path given by `locator`"
        path = locator.format(self).split('/')
        return Scope.getProcessScope( *path, get_or_create=True )

    @property
    def hooks( self ):
        "return hook scope"
        return self._v_hooks


    def __init__( self, rootId = None, **kwArgs ):
        super(Simulator, self).__init__( kwArgs )
        self.initialize( rootId )
        # update instance attributes ...
        self.__dict__.update( dict( self.__defaults__, **kwArgs ) )


    def initialize( self, rootId ):
        """initialize simulator by clearing state and declaring hooks.
        If called repeatedly, do a reinitialization if `reinit` is True otherwise do nothing.
        Return True if reinitialization is done.
        """
        self._rootId  = (rootId or self._rootId).format( self )
        self._v_state = self._get_path_scope( self._stateLocator )
        self._v_hooks = self._get_path_scope( self._hooksLocator )
        self._v_state.clear()
        self._v_hooks.clear()
        # declare hooks ...
        for hid,t in self.__hooks__.items():
            self._v_hooks.declareCallback( hid, t )


    def setupState( self, state = {}, **kwArgs ):
        self.state = self.exposeDomain( self._v_state, dict( self.__state__, **state ), 'state' )
        # convert given values to state types ...
        kwArgs = { k: type(self.state[k])(v) for k,v in kwArgs.items() }
        self._v_state.updateDomain( kwArgs )
        self.callHook( 'initialization_done' )
        return self.state


    def declareStateDomain( self, ident, domain ):
        self.__dict__[ident] = self.exposeDomain( self._v_state.getSubScope(ident), domain, ident )


    def exposeDomain( self, root, domain, ident ):
        def _walk( items, scope, ident ):
            obj = mkTypeObject( ident )
            for k,v in items:
                try:
                    setattr( obj, k, _walk( v.items(), scope.getSubScope(k), k ) )
                except AttributeError:
                    scope[k] = v                    #< set value in scope ...
                    var      = Variable( scope[k] ) #< before building Variable from retrieved reference
                    setattr( obj, '_v_' + k, var )
                    setattr( type(obj), k, var.mkProperty() )
            return obj
        #
        return _walk( domain.items(), root, ident )


    def callHook( self, ident, arg = None ):
        self._v_hooks.invokeCallback( ident, arg )
