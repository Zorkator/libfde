
from ._expression import ContextEvaluable


#-----------------------------------------------
class DeferredType( type(ContextEvaluable) ):
#-----------------------------------------------

    def eval( _class, ident: str ):
        return DeferredEval( ident, (_class._globals, _class._locals) )

    def wrap( _class, obj ):
        if isinstance( obj, str ): return _class.eval( obj )
        else                     : return Deferred( obj )


#-------------------------------------------------------------
class Deferred( ContextEvaluable, metaclass=DeferredType ):
#-------------------------------------------------------------
    __slots__ = ('_obj', '_data')

    def __init__( self, obj, data = None ):
        self._obj  = obj
        self._data = data

    def __getattr__( self, ident ):
        return DeferredAttr( self, ident )

    def __getitem__( self, item ):
        return DeferredSubscript( self, item )

    def __call__( self, *args, **kwArgs ):
        return DeferredCall( self, (args, kwArgs) )

    def __str__( self ):
        return getattr( self._obj, '__name__', None ) or str(self._obj)

    def __value__( self ):
        return self.__resolve__( self._obj )

    @classmethod
    def __resolve__( _class, obj ):
        if isinstance( obj, Deferred ):
            return obj.__value__()
        try:   return {k: _class.__resolve__( v ) for k, v in obj.items()}       #< try mapping
        except AttributeError:
            try                  : iter(obj), obj.strip
            except TypeError     : pass                                          #< non-iterable
            except AttributeError: return [_class.__resolve__( a ) for a in obj] #< non-string iterable
        return obj


#----------------------------------
class DeferredAttr( Deferred ):
#----------------------------------
    def __value__( self ):
        obj = self.__resolve__( self._obj )
        return getattr( obj, self.__resolve__( self._data ) )

    def __str__( self ):
        return '{0._obj}.{0._data}'.format( self )


#----------------------------------
class DeferredCall( Deferred ):
#----------------------------------
    def __value__( self ):
        obj = self.__resolve__( self._obj )
        return obj( *self.__resolve__( self._data[0] ), **self.__resolve__( self._data[1] ) )

    def __str__( self ):
        args = ', '.join( (*map( str, self._data[0] ), *(f'{k}={v}' for k,v in self._data[1].items())) )
        return '{0._obj}( {1} )'.format( self, args )


#--------------------------------------
class DeferredSubscript( Deferred ):
#--------------------------------------
    def __value__( self ):
        obj = self.__resolve__( self._obj )
        return obj[self.__resolve__( self._data )]

    def __str__( self ):
        return '{0._obj}[ {0._data} ]'.format( self )


#---------------------------------
class DeferredEval( Deferred ):
#---------------------------------
    def __value__( self ):
        obj = self.__resolve__( self._obj )
        return eval( obj, *self._data )

    def __str__( self ):
        return 'eval({0._obj})'.format( self )
