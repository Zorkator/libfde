
from ._nativeController import cached_property

######################################
class Hookable(object):
######################################
    """Mixin class extending FDEController types.

    Hookable provides cashed access to certain hook scope, determined by option hooksPath

    """
    __opts__      = dict( hooksPath = '{rootId}/hooks' )
    __hookAlias__ = dict()


    @cached_property
    def hooks( self ):
        """return hook scope, specified by option hooksPath."""
        from fde.core import Scope
        path = self._hooksPath.format( **self.about ).split('/')
        return Scope.getProcessScope( *path )


    @property
    def activeHookCount( self ):
        return sum( map( bool, self.hooks.values() ) )


    def initialize( self, **kwArgs ):
        super(Hookable, self).initialize( **kwArgs )

        #----
        # connect callback hooks ...
        #
        hooks = self.hooks
        # first, connect methods implemented directly in class ...
        for hookId in hooks.keys():
            try   : hooks.setCallback( hookId, getattr( self, hookId ) )
            except: pass

        # then, connect methods decorated with hookIds ...
        Ctrl  = type(self)
        mapId = self.__hookAlias__.get
        # NOTE: get Controller-methods from class, to prevent triggering property evaluation!
        for method in filter( callable, [ getattr( Ctrl, m ) for m in dir(Ctrl) ] ):
            for hookId in getattr( method, '_hookIds', [] ):
                hooks.connectCallback( mapId( hookId, hookId ), method.__get__( self ) )
        #
        # THUS, methods get called in this order!



def connect_to_hook( *hookIds ):
    """mark method for connecting it to given list of hookIds."""
    def _decorate( m ):
        m._hookIds = hookIds
        return m
    return _decorate

