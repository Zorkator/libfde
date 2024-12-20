
import math

#--------------------------------
class Evaluable( object ):
#--------------------------------
    @property
    def value( self ):
        return self.__value__()

    def __value__( self ):
        raise NotImplementedError

    def __bool__( self ):  #< CAUTION: if you override this in a subclass ...
        """return bool-conversion of Expression-value.
        CAUTION: in contrast to normal bool-conversion NaN is considered False.
        """
        v = self.value
        return bool(v) and not math.isnan(v)

    __nonzero__ = __bool__ #< ... you've to redefine this as well!

    @classmethod
    def subclass( _class, **kwArgs ):
        return type( _class.__name__, (_class,), kwArgs )


#--------------------------------------
class ContextEvaluable( Evaluable ):
#--------------------------------------
    #           . O O (None uses python's default)
    _globals = None
    _locals  = None
    _context = None  # < set via subclass

    @property
    def globals( self ):
        return self._globals

    @property
    def locals( self ):
        return self._locals

    @property
    def context( self ):
        return self._context

    @classmethod
    def subclass( _class, context ):
        return super(ContextEvaluable, _class).subclass( _context=context, _globals=context.globals, _locals=context.locals )


#---------------------------------------
class Expression( ContextEvaluable ):
#---------------------------------------

    def __init__( self, expr, *args, **kwArgs ):
      self._expr = getattr( expr, '_expr', expr )
      self._code = getattr( expr, '_code', None ) \
                or compile( self._expr, type(self).__name__, 'eval' )

    def __value__( self ):
        return eval( self._code, self._globals, self._locals )

    def __str__( self ):
        return self._expr



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
        try:
            len(obj)
            try:
                obj[:]
                if not hasattr(obj, 'strip'):
                    return [_class.__resolve__(a) for a in obj]
            except TypeError:
                return {k:_class.__resolve__(v) for k, v in obj.items()}
        except TypeError:
            pass
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
