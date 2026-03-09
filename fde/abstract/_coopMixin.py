
from abc import ABCMeta, abstractmethod


#-------------------------------------------
class _mixin( object ):
#-------------------------------------------

    def __init__( self, *req ):
        self._req = req

    def __call__( self, cls ):
        if self._req or not isinstance( cls, MixinType ):
            # inspired by six.add_metaclass...
            members = dict( cls.__dict__, __qualname__=cls.__qualname__, __coop_bases__=self._req )
            slots   = members.get( '__slots__', () )
            if isinstance( slots, str ):
                slots = (slots,)
            [members.pop(i, None) for i in (*slots, '__dict__', '__weakref__')]
            cls = MixinType( cls.__name__, cls.__bases__, members )
        return cls

    def requires( self, *req ):
        return type(self)( *self._req, *req )

    def employ( self, mixins, sequential = False ):
        cls = next( iter(self._req), None )
        if sequential:
            for m in mixins:
                cls = type( cls )( m.__name__ + cls.__name__, (m, cls), {} )
        elif mixins:
            cls = type(cls)( ''.join( m.__name__ for m in mixins ) + cls.__name__, (*mixins, cls), {} )
        return cls


#-------------------------------------------
class MixinType( ABCMeta ):
#-------------------------------------------

    @staticmethod
    def _ensureMarker( enable, cls: type, ident: str ):
        hasMarker = getattr( getattr( cls, ident, None ), '__isabstractmethod__', None )
        if enable:
            if not hasMarker:
                yield ident, abstractmethod( lambda: None )
        else:
            if hasMarker:
                yield ident, lambda: None

    def __new__( _class, name, bases, members ):
        from itertools import chain
        cls   = super(MixinType, _class).__new__( _class, name, bases, members )
        coops = set( chain( *(vars(b).get( '__coop_bases__', () ) for b in cls.mro()) ) )
        if coops:
            members['__missing_coops__'] = missing = tuple( c for c in coops if not issubclass( cls, c ) )
            members.update( _class._ensureMarker( missing, cls, '\xa0\b<missing superclasses>' ) )
            cls = super( MixinType, _class ).__new__( _class, name, bases, members )
        cls.mixin = _mixin( cls )
        return cls

    @property
    def missing_coops( _class ):
        return getattr( _class, '__missing_coops__', () )


mixin = _mixin()
