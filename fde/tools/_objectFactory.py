
from weakref import WeakValueDictionary

#----------------------------------
class ObjectFactory( object ):
#----------------------------------

    def __init__( self, objCreator, identOp = None, dictType = WeakValueDictionary ):
        self._objCreator = objCreator
        self._instances  = dictType()
        self._identOp    = identOp or id

    def __call__( self, *args, **kwArgs ):
        obj = self._objCreator( *args, **kwArgs )
        self._instances[ self._identOp( obj ) ] = obj
        return obj

    def __getitem__( self, ident ):
        return self._instances[ident]

    @property
    def count( self ):
        return len( self._instances )

    @property
    def vars( self ):
        return self._instances.values()

    @property
    def create( self ):
        return self


#-------------------------------------------
class NamedObjectFactory( ObjectFactory ):
#-------------------------------------------

    def __init__( self, objCreator, identOp = None, dictType = WeakValueDictionary ):
        super(NamedObjectFactory, self).__init__( objCreator, identOp or (lambda i: i), dictType )

    def __call__( self, ident, *args, **kwArgs ):
        ident = self._identOp( ident )
        try   : return self._instances[ident]
        except:
            self._instances[ident] = obj = self._objCreator( ident, *args, **kwArgs )
            return obj

    def __getitem__( self, ident ):
        return self._instances[ self._identOp( ident ) ]

    @property
    def pairs( self ):
        return self._instances.items()

    @property
    def names( self ):
        return self._instances.keys()
