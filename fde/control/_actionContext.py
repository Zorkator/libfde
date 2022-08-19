
from ._expression import Evaluable, Expression
from ..tools      import _decorate
import re

#--------------------------------------------
class Trigger( Expression ):
#--------------------------------------------
    _str1     = '"[^"]+"'
    _str2     = "'[^']+'"
    _other    = "[^\"'\s]+"
    _regEx    = '(%s|%s|%s)' % (_str1, _str2, _other)
    _strTokOp = '__lookup__({})'.format
    _context  = None #< set via subclass

    @property
    def context( self ):
        return self._context

    def __init__( self, expr, **kwArgs ):
        tokens = []
        for t in re.findall( self._regEx, expr ):
            if t[0] in '\'"':
                t = self._strTokOp( t )
            tokens.append( t )
        super(Trigger, self).__init__( ' '.join( tokens ) )
        self.__dict__.update( _decorate( kwArgs.items() ) )

    @classmethod
    def subclass( _class, context ):
        return super(Trigger, _class).subclass( _context=context, _globals=context.globals, _locals=context.locals )


#--------------------------------------------
class Action( Evaluable ):
#--------------------------------------------
    _context = None #< set via subclass

    @property
    def context( self ):
        return self._context

    @property
    def cause( self ):
        return self._cause

    def __init__( self, cause, func, *args, **kwArgs ):
        if not callable(func):
            raise AssertionError( "Action argument 3 must be callable, got %s instead!" % type(func) )
        self._cause  = cause
        self._func   = func
        self._args   = args
        self._kwArgs = kwArgs

    def __value__( self ):
        return self._func( self, *self._args, **self._kwArgs )

    def evaluate( self ):
        if self._cause:
            return self.value

    @classmethod
    def subclass( _class, context ):
        return super(Action, _class).subclass( _context=context )



#--------------------------------------------
class ActionContext(object):
#--------------------------------------------
    Action   = Action
    Trigger  = Trigger
    _globals = dict( __builtins__ = None )

    @property
    def globals( self ):
        return self._globals

    @property
    def locals( self ):
        return self._locals

    @property
    def host( self ):
        return self._host


    def __init__( self, host, varLookup = None, globals = None, locals = None ):
        varLookup = varLookup or (lambda i: i)

        self._host    = host
        self._locals  = dict()                if (locals  is None) else locals
        self._globals = dict( self._globals ) if (globals is None) else globals

        # subclass classes Action and Trigger to make own ones for the created ActionContext
        self.Action  = self.Action.subclass( self )
        self.Trigger = self.Trigger.subclass( self )
        self.lookup  = varLookup
        self._globals.update( Action=self.Action, Trigger=self.Trigger, __lookup__=varLookup )


    def exec( self, code ):
        exec( code, self._globals, self._locals )

    def exec_str( self, codeStr, origin = '__str__' ):
        self.exec( compile( codeStr, origin, 'exec' ) )

    def exec_file( self, filename ):
        with open( filename ) as f:
            self.exec_str( f.read(), f.name )
