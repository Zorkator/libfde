
import weakref

#-------------------------------------------
class WeakList(list):
#-------------------------------------------

    def _ref( self, *obj ):
        cb = super(WeakList, self).remove
        return [ weakref.ref( o, cb ) for o in obj ]

    def __init__( self, items = () ):
        super(WeakList, self).__init__( self._ref( *items ) )

    def __repr__( self ):
        return "{}({})".format( type(self).__name__, list(self) )

    def __iter__( self ):
        return (ref() for ref in super(WeakList, self).__iter__())

    def __getitem__( self, idx ):
        res = super(WeakList, self).__getitem__( idx )
        if isinstance( idx, slice ): return [ref() for ref in res]
        else                       : return res()

    def __setitem__( self, idx, obj ):
        if isinstance( idx, slice ): obj = self._ref( *obj )
        else                       : obj = self._ref( obj )[0]
        super(WeakList, self).__setitem__( idx, obj )

    def __add__( self, other ):
        return type(self)( list(self) + list(other) )

    def __iadd__( self, other ):
        self.extend( other )
        return self

    def __mul__( self, n ):
        return type(self)( list(self) * n )

    def __imul__( self, n ):
        return type(self)( list(self) * n )

    def append( self, obj ):
        super(WeakList, self).append( self._ref( obj )[0] )

    def extend( self, items ):
        super(WeakList, self).extend( self._ref( *items ) )

    def count( self, obj ):
        return list(self).count( obj )

    def index( self, obj ):
        return list(self).index( obj )

    def insert( self, idx, obj ):
        super(WeakList, self).insert( idx, self._ref( obj )[0] )

    def remove( self, obj, firstOnly = True ):
        # generate ascending list of indices where obj is found ...        ... and take firstOnly or all
        indices = [ idx for idx, item in enumerate( self ) if item == obj ][:firstOnly or None]
        for idx in indices[::-1]: #< remove indices in reversed order to avoid shifts!
            del self[idx]

    def sort( self, *args ):
        self[:] = list(self).sort( *args )
