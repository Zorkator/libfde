
import re

#####################################
class Expression(object):
#####################################
    _str1     = '"[^"]+"'
    _str2     = "'[^']+'"
    _other    = "[^\"'\s]+"
    _regEx    = '(%s|%s|%s)' % (_str1, _str2, _other)
    _strTokOp = '__lookup__({})'.format

    # by setting None, use python's default ...
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


    @property
    def value( self ):
        return self.__value__()

    def __value__( self ):
        return eval( self._code, self._globals, self._locals )


    def __bool__( self ):  #< CAUTION: if you override this in a subclass ...
        return bool( self.value )

    __nonzero__ = __bool__ #< ... you've to redefine this as well!

    def __str__( self ):
        return self._expr

