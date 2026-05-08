
from ._expression import ProtoClass, Evaluable, Expression
from ..tools      import _decorate, Caching, cached_property
from itertools    import chain
from pathlib      import Path
import re

#--------------------------------------------
class Trigger( Expression ):
#--------------------------------------------
    _str1     = r'"[^"]+"'
    _str2     = r"'[^']+'"
    _other    = r"[^\"'\s]+"
    _regEx    = '(%s|%s|%s)' % (_str1, _str2, _other)
    _strTokOp = '__lookup__({})'.format

    @property
    def context( self ):
        return self._context

    def __init__( self, expr, **kwArgs ):
        if not isinstance( expr, Expression ):
            # assume str-type ...
            tokens = []
            for t in re.findall( self._regEx, expr ):
                if t[0] in '\'"':
                    t = self._strTokOp( t )
                tokens.append( t )
            expr = ' '.join( tokens )
        super(Trigger, self).__init__( expr )
        self.__dict__.update( _decorate( kwArgs.items() ) )


#--------------------------------------------
class Action( Evaluable ):
#--------------------------------------------
    _context = None #< set via subclass

    @classmethod
    def subclass( _class, context ):
        return super(Action, _class).subclass( _context=context )

    @property
    def context( self ):
        return self._context

    @property
    def cause( self ):
        return self._cause

    def __init__( self, cause, func, *args, **kwArgs ):
        if not callable(func):
            raise AssertionError( "Action argument 3 must be callable, got %s instead!" % type(func) )
        self._cause  = self._context.Trigger( cause )
        self._func   = func
        self._args   = args
        self._kwArgs = kwArgs

    def __value__( self ):
        return self._func( self, *self._args, **self._kwArgs )

    def evaluate( self ):
        if self._cause:
            return self.value



#--------------------------------------------
class ActionContext( ProtoClass ):
#--------------------------------------------
    Action     = Action
    Trigger    = Trigger
    Expression = Expression
    _globals   = dict( __builtins__ = {} )

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
        self._host    = host
        self._locals  = dict()                if (locals  is None) else locals
        self._globals = dict( self._globals ) if (globals is None) else globals

        # subclass classes Action and Trigger to make own ones for the created ActionContext
        self.Action     = self.Action.subclass( self )
        self.Trigger    = self.Trigger.subclass( self )
        self.Expression = self.Expression.subclass( self )
        self.lookup     = varLookup or (lambda i: i)
        self._globals.update( Action=self.Action, Trigger=self.Trigger, __lookup__=self.lookup )

    def iter( self, type: type = object, globals: bool = False, locals: bool = True ):
        for k, v in chain( ({},self._globals)[globals].items(), ({},self._locals)[locals].items() ):
            if isinstance( v, type ):
                yield k, v

    def eval_code( self, code: (str | object) ):
        return eval( code, self._globals, self._locals )

    def exec_code( self, code: (str | object) ):
        exec( code, self._globals, self._locals )

    def exec_file( self, file: (Path, str) ):
        file = Path( file )
        self.exec_code( compile( file.read_text(), file.name, 'exec' ) )

    def eval_or_exec( self, cmd ):
        try:
            return self.eval_code( cmd )
        except SyntaxError:
            self.exec_code( cmd )

    # decorator
    @classmethod
    def config( _class, Trigger = None, Action = None, globals = None, **other ):
        def _extender( cls ):
            _g = dict( dict( _class._globals, **globals or {} ), **other )
            setattr( cls, _class.__name__, _class.subclass_omit( None, Trigger=Trigger, Action=Action, _globals=_g ) )
            return cls
        return _extender



#-------------------------------------------------
class ActionContextHost( ProtoClass, Caching ):
#-------------------------------------------------
    ActionContext = ActionContext
    commandPrefix = None
    usedBuiltins  = []

    @cached_property
    def actionContext( self ):
        """return default ActionContext object, using class types for Action, Trigger and VariableLookup."""
        return self.makeActionContext( self.Var, self.commandPrefix, self.usedBuiltins )


    def makeActionContext( self, varLookup = None, cmdPrefix = None, usedBuiltins = [] ):
        """return new action context, using custom or default VariableLookup.
        If cmdPrefix is given, all methods or properties starting with it will be added without prefix as global symbols.
        """
        context = self.ActionContext( self, varLookup )
        if cmdPrefix:
            selfType = type( self )
            startIdx = len(cmdPrefix)
            members  = [(m[startIdx:], getattr( selfType, m )) for m in dir( selfType ) if m.startswith( cmdPrefix )]
            commands = {i: getattr( m, 'fget', m ).__get__( self ) for i, m in members}  # < treat cmd-properties the same
            context.globals.update( commands )
        context.globals.update( {i: __builtins__[i] for i in set( __builtins__ ).intersection( usedBuiltins )} )
        return context


    @cached_property
    def Var( self ):
        """return variable lookup object."""
        return self.makeVariableLookup()


    def makeVariableLookup( self, rootScope = None, keyTok = None, varType = None ):
        """return new variable factory that does lookups and creates <varType> from the result.
        """
        return None


    def evalCommand( self, cmd ):
        """evaluate or execute the provided string `cmd`.
        If `cmd` represents an expression, return its result or in case of a statement, return None.
        """
        return self.actionContext.eval_or_exec( cmd )
