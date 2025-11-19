
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 5)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = "Object TypedObject TypeInfo String Item Ref List HashMap Scope".split()

for classId in __all__:
    globals()[classId] = type( classId, (object,), {} )


from functools import wraps, partial
from itertools import chain
from abc       import ABCMeta, abstractmethod
from six       import add_metaclass

class MixinType( ABCMeta ):
    pass

#-------------------------------------------
class _mixin( metaclass=MixinType ):
#-------------------------------------------
    # @property
    # @abstractmethod
    # def __required_bases__( self ):
    #     ...

    def __init_subclass__( _class, type = None, **kwArgs ):
        super().__init_subclass__( **kwArgs )
        if type is not _mixin:
            required = set( chain( *(vars(c).get('__required_bases__', ()) for c in _class.mro()[:-1]) ) )
            if len( missing := [c for c in required if not issubclass( _class, c )] ):
                setattr( _class, '__missing_bases__', missing )
                _class.__abstractmethods__ = frozenset({'__missing_bases__'}) #< TODO: make abstract property instead?
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
