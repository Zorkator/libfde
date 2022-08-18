
from ._expression import Expression
from ..tools      import _decorate
import re

#--------------------------------------------
class _Trigger(Expression):
#--------------------------------------------
    _str1     = '"[^"]+"'
    _str2     = "'[^']+'"
    _other    = "[^\"'\s]+"
    _regEx    = '(%s|%s|%s)' % (_str1, _str2, _other)
    _strTokOp = '__lookup__({})'.format

    context   = None #< set by ActionContext

    def __init__( self, expr, **kwArgs ):
        tokens = []
        for t in re.findall( self._regEx, expr ):
            if t[0] in '\'"':
                t = self._strTokOp( t )
            tokens.append( t )
        super(_Trigger, self).__init__( ' '.join( tokens ) )
        self.__dict__.update( _decorate( kwArgs.items() ) )



#--------------------------------------------
class _Action(object):
#--------------------------------------------
    _instances = []
    context    = None #< set by ActionContext

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

        type(self)._instances.append( self )


    def evaluate( self ):
        if self._cause:
            self.execute()


    def execute( self ):
        return self._func( self, *self._args, **self._kwArgs )


    @classmethod
    def evaluateAll( _class ):
        for a in _class._instances:
            a.evaluate()



#--------------------------------------------
class ActionContext(object):
#--------------------------------------------
    Trigger  = _Trigger
    Action   = _Action
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
        varLookup = varLookup or (lambda i : i)

        self._host    = host
        self._locals  = dict()                if (locals  is None) else locals
        self._globals = dict( self._globals ) if (globals is None) else globals
        self._globals['__lookup__'] = varLookup

        # Derive given classes to make own ones for the newly created ActionContext

        #----------------------------------
        class Action(self.Action):
        #----------------------------------
            _instances = []
            context    = self

        #----------------------------------
        class Trigger(self.Trigger):
        #----------------------------------
            _globals = self._globals
            _locals  = self._locals
            context  = self

        self.lookup  = varLookup
        self.Trigger = Trigger
        self.Action  = Action
        self._globals.update( Trigger = Trigger, Action = Action )


    def __call__( self, *args, **kwArgs ):
        return self.create( *args, **kwArgs )


    def execute( self, code ):
        exec( code, self._globals, self._locals )
