
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
        return super(Expression, _class).subclass( _context=context, _globals=context.globals, _locals=context.locals )


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

