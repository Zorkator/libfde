
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
    def __init__( self, kwSeq = {}, **kwArgs ):
        self.__dict__.update( kwSeq, **kwArgs )


#-------------------------------------------
class TypeObject( object ):
#-------------------------------------------

    def __init__( self, kwIter = {}, **kwArgs ):
        self.__update__( kwIter, **kwArgs )

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
        if hasattr( key, 'strip' ): #< TODO: handle key variants by TypeError!
            try   : setattr( self, key, type(self)( {k:value[k] for k in value} ) )
            except: setattr( self, key, value )
        else:
            [ setattr( self, *kv ) for kv in zip(key,value) ]

    def __update__( self, kwIter = {}, **kwArgs ):
        def _walk( itr, stack ):
            for key, v in itr:
                try                  : _walk( v.items(), stack + [stack[-1][key]] )
                except AttributeError: stack[-1][key] = v
        # assign mappings one after another to merge nested mappings!
        kwIter and _walk( dict(kwIter).items(), [self] )
        kwArgs and _walk( kwArgs.items(), [self] )

    def __getstate__( self ):
        return {k: (v.__getstate__() or v) for k, v in zip( self, self[self] )}

    def __setstate__( self, state ):
        vars(self).clear()
        self.__update__( state )


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
