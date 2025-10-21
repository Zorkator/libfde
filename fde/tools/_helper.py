
#-------------------------------------------
class NullHandle( object ):
#-------------------------------------------
    def __null_method( self, *args, **kwArgs ):
        pass

    def __getattr__( self, name ):
        setattr( self, name, type(self).__null_method )
        return self.__null_method


# -------------------------------------------
class Wallet( object ):
#-------------------------------------------
    def __init__( self, members=None ):
        if members is not None:
            self.__dict__.update( members )


#-------------------------------------------
class TypeObject( object ):
#-------------------------------------------
    def __init__( self, kwIter = {}, **kwArgs ):
        self.__dict__.update( **dict( kwIter, **kwArgs ) )


    def __iter__( self ):
        return iter(self.__dict__)


    def __contains__( self, key ):
        if hasattr( key, 'strip' ):
            return hasattr( self, key )
        else:
            return all( hasattr( self, k ) for k in key )


    def __getitem__( self, key ):
        if hasattr( key, 'strip' ):
            return getattr( self, key )
        else:
            return (getattr( self, k ) for k in key)


    def __setitem__( self, key, value ):
        if hasattr( key, 'strip' ):
            setattr( self, key, value )
        else:
            assert len(key) == len(value), "mismatch in number of keys and values"
            [ setattr( self, *kv ) for kv in zip(key,value) ]

    def __getstate__( self ):
        return dict( zip( self, self[self] ) )

    def __setstate__( self, state ):
        self[state.keys()] = state.values()


def mkTypeObject( ident, bases = (TypeObject,), members = {} ):
    "create and return instance of newly created class `ident` (upper-cased), inheriting `bases` and owning `members`."
    return type( ident[:1].upper() + ident[1:], tuple(bases), members )()


#-------------------------------------------
class NullGuard( object ):
#-------------------------------------------
    def __init__( self, *args, **kwArgs ):
        self.__dict__.update( kwArgs, args = args )

    def __enter__( self ):
        return self

    def __exit__( self, *args ):
        return


#-------------------------------------------
class _arg( object ):
#-------------------------------------------
    def __init__( self, default ):
        self._default = default

    @classmethod
    def isGiven( _class, arg ):
        return not isinstance( arg, _class )

    @classmethod
    def get( _class, arg ):
        if isinstance( arg, _class ): return arg._default
        else                        : return arg


def auto_raise( obj, what = None ):
    if isinstance( obj, type ) and issubclass( obj, Exception ): raise obj( what )
    if isinstance( obj, Exception )                            : raise obj
    return obj


def _decorate( kvPairs, **kwArgs ):
    for k, v in dict( kvPairs, **kwArgs ).items():
        yield ('_' + k, v)


from functools import wraps, partial
from itertools import chain
from abc       import ABCMeta, abstractmethod
from six       import add_metaclass

#-------------------------------------------
class _mixin( metaclass=ABCMeta ):
#-------------------------------------------
    @property
    @abstractmethod
    def __required_bases__( self ):
        ...

    def __init_subclass__( _class, type = None, **kwArgs ):
        super().__init_subclass__( **kwArgs )
        if type is not _mixin:
            required = set( chain( *(vars(c).get('__required_bases__', ()) for c in _class.mro()[:-1]) ) )
            if len( missing := [c for c in required if not issubclass( _class, c )] ):
                setattr( _class, '__missing_bases__', missing )
                _class.__abstractmethods__ = frozenset({'__missing_bases__'})
                _class
                #raise TypeError( f'Mixin class {_class.__name__} must inherit from {", ".join(missing)}.' )

    # decorator
    @classmethod
    def requires( _class, *reqClasses ):
        return partial( _class.__tool, reqClasses )

    @staticmethod
    def __tool( reqClasses, cls ):
        if not issubclass( cls, _mixin ):
            cls = wraps(cls, updated=())( type( '#', (_mixin,) + cls.__bases__, {'__required_bases__': reqClasses}, type=_mixin ) )
        return cls


def mixin( cls ):
    return _mixin.requires()( cls )

vars(mixin)['requires'] = _mixin.requires
