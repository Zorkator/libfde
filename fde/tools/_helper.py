
#-------------------------------------------
class NullHandle( object ):
#-------------------------------------------
    def __null_method( self, *args, **kwArgs ):
        pass

    def __getattr__( self, name ):
        setattr( self, name, type(self).__null_method )
        return self.__null_method


#-------------------------------------------
class Wallet( object ):
#-------------------------------------------
    def __init__( self, kwSeq = {}, **kwArgs ):
        self.__dict__.update( kwSeq, **kwArgs )


#-------------------------------------------
class TypeObject( object ):
#-------------------------------------------
    """
    A recursive dictionary-like object that allows access via attributes.

    Key Features:
    - **Dot Access:** Attributes can be accessed as `obj.key` or `obj['key']`.
    - **Batch Operations:** Get, Set, and Delete operations support lists of keys
      to perform actions in batch.
    """
    def __init__( self, kwSeq = {}, **kwArgs ):
        self.__dict__.update( kwSeq, **kwArgs )


    def __iter__( self ):
        return iter(self.__dict__)


    def __len__( self ):
        return len(self.__dict__)


    def __contains__( self, key ):
        try:
            return hasattr( self, key )
        except TypeError:
            return all( hasattr( self, k ) for k in key )


    def __getitem__( self, key ):
        try:
            return getattr( self, key )
        except TypeError:
            return (getattr( self, k ) for k in key)


    def __setitem__( self, key, value ):
        try:
            setattr( self, key, value )
        except TypeError:
            assert len(key) == len(value), "mismatch in number of keys and values"
            [setattr( self, *kv ) for kv in zip(key, value)]

    def __getstate__( self ):
        return dict( zip( self, self[self] ) )

    def __setstate__( self, state ):
        self[state.keys()] = state.values()



# So far TypeObject doesn't define any non-magic methods to avoid name clashes.
# However, for compatibility with **-unpacking or dict-converting TypeObjects we have to add a keys-method.
# Actually, this smells like a bug in Python, since __iter__ and __getitem__ should be enough!
try:
    dict( **TypeObject() )
except TypeError:
    TypeObject.keys = TypeObject.__iter__



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
