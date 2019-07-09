
import re

#####################################
class Expression(object):
#####################################
    _str1     = '"[^"]+"'
    _str2     = "'[^']+'"
    _other    = "[^\"'\s]+"
    _regEx    = '(%s|%s|%s)' % (_str1, _str2, _other)
    _strTokOp = '__lookup__({})'.format

    # factory updated class properties
    _globals = None
    _locals  = None

    def __init__( self, expr, *args, **kwArgs ):
        tokens = []
        for t in re.findall( self._regEx, expr ):
            if t[0] in '\'"':
                t = self._strTokOp( t )
            tokens.append( t )
        self._expr = ' '.join( tokens )
        self._code = compile( self._expr, type(self).__name__, 'eval' )


    def eval( self ):
        return eval( self._code, self._globals, self._locals )

    def __bool__( self ):
        return bool( self.eval() )

    __nonzero__ = __bool__

    def __str__( self ):
        return self._expr



##########################################
class ExpressionContext(object):
##########################################

    @property
    def globals( self ):
        return self._globals

    @property
    def locals( self ):
        return self._locals

    def __init__( self, exprType = Expression, varLookup = None, globals = None, locals = None ):
        self._globals = dict( __builtins__ = None ) if (globals is None) else globals
        self._locals  = dict()                      if (locals  is None) else locals
        self._globals['__lookup__'] = varLookup or (lambda i : i)

        class _Expression(exprType):
            _globals = self._globals
            _locals  = self._locals

        self.create = _Expression
        self.lookup = self.globals['__lookup__']


    def __call__( self, *args, **kwArgs ):
        return self.create( *args, **kwArgs )


    def execute( self, code ):
        exec( code, self._globals, self._locals )

