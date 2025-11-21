
from abc       import ABCMeta, abstractmethod
from functools import partial


#-------------------------------------------
class _mixin( object ):
#-------------------------------------------

    @classmethod
    def decorator( _class, *reqClasses ):
        dec          = partial( _class._transform, reqClasses )
        dec.requires = partial( _class._requires, *reqClasses )
        return dec

    @classmethod
    def _requires( _class, *reqClasses ):
        return partial( _class._transform, reqClasses )

    @staticmethod
    def _transform( reqClasses, cls ):
        if not issubclass( type(cls), MixinType ):
            cls = MixinType( cls.__name__, cls.__bases__, dict( cls.__dict__, __coop_bases__=reqClasses ) )
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
        cls.mixin = _mixin.decorator( cls )
        return cls

    @property
    def missing_coops( _class ):
        return getattr( _class, '__missing_coops__', () )


mixin = _mixin.decorator()
