
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
        return Deferred( ident, _eval=(_class._globals, _class._locals) )

    def wrap( _class, obj ):
        if isinstance( obj, str ): return _class.eval( obj )
        else                     : return Deferred( obj )


#-------------------------------------------------------------
class Deferred( ContextEvaluable, metaclass=DeferredType ):
#-------------------------------------------------------------
    __slots__ = ('_obj',)
    _attr = ''
    _args = None
    _item = None
    _eval = None

    def __init__( self, obj, **kwArgs ):
        self._obj = obj
        vars(self).update( kwArgs )

    def __getattr__( self, ident ):
        return type(self)( self, _attr=ident )

    def __getitem__( self, item ):
        return type(self)( self, _item=item )

    def __call__( self, *args, **kwArgs ):
        return type(self)( self, _args=(args, kwArgs) )

    def __str__( self ):
        def _recurse( obj ):
            if isinstance( obj, Deferred ):
                return _recurse( obj._obj ) + str(vars(obj))
            return getattr( obj, '__name__', None ) or str(obj)
        return _recurse( self )

    def __value__( self ):
        obj = self.__resolve__( self._obj )
        if self._attr:
            return getattr( obj, self.__resolve__( self._attr ) )
        if self._args:
            return obj( *self.__resolve__( self._args[0] ), **self.__resolve__( self._args[1] ) )
        if self._eval:
            return eval( obj, *self._eval )
        if self._item:
            return obj[ self.__resolve__( self._item ) ]
        return obj

    @classmethod
    def __resolve__( _class, obj ):
        if isinstance( obj, Deferred ):
            return obj.value
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
