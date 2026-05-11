
from math import isnan


#-----------------------------
class ProtoClass( object ):
#-----------------------------
    @classmethod
    def subclass( _class, *mixins, **kwArgs ):
        return type( _class.__name__, (*mixins, _class), kwArgs )

    @classmethod
    def subclass_omit( _class, omit, *mixins, **kwArgs ):
        kwArgs = dict( i for i in kwArgs.items() if i[1] != omit )
        return _class.subclass( *mixins, **kwArgs )


#--------------------------------
class Evaluable( ProtoClass ):
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
        return bool(v) and not isnan(v)

    __nonzero__ = __bool__  #< ... you've to redefine this as well!


#--------------------------------------
class ContextEvaluable( Evaluable ):
#--------------------------------------
    #           . O O (None uses python's default)
    _globals = None
    _locals  = None
    _context = None  #< set via subclass

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
