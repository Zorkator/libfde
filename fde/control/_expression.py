
import math

#--------------------------------
class Evaluable(object):
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


#--------------------------------
class Expression( Evaluable ):
#--------------------------------
    #           . O O (None uses python's default)
    _globals = None
    _locals  = None

    @property
    def globals( self ):
        return self._globals

    @property
    def locals( self ):
        return self._locals

    def __init__( self, expr, *args, **kwArgs ):
      self._expr = expr
      self._code = compile( expr, type( self ).__name__, 'eval' )

    def __value__( self ):
        return eval( self._code, self._globals, self._locals )

    def __str__( self ):
        return self._expr
