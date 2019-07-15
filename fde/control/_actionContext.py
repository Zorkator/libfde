
from ._expression import Expression
from ..tools      import _decorate

##########################################
class Trigger(Expression):
##########################################

  def __init__( self, expr, **kwArgs ):
    super(Trigger,self).__init__( expr )
    self.__dict__.update( _decorate( kwArgs.items() ) )



##########################################
class Action(object):
##########################################
    _varLookup = lambda i: i
    _instances = []

    def __init__( self, tgt, cause, func, *args, **kwArgs ):
        try                  : self._tgt = self._varLookup( tgt.strip() ) #< try tgt given as string
        except AttributeError: self._tgt = tgt                            #< else assume reference

        if not callable(func):
            raise AssertionError( "Action argument 3 must be callable, got %s instead!" % type(func) )

        self._cause  = cause
        self._func   = func
        self._args   = args
        self._kwArgs = kwArgs

        type(self)._instances.append( self )


    def evaluate( self ):
        if self._cause:
            self._tgt.value = self._func( self, *self._args, **self._kwArgs )


    @classmethod
    def evaluateAll( _class ):
        for a in _class._instances:
            a.evaluate()



##########################################
class ActionContext(object):
##########################################
    Trigger = Trigger
    Action  = Action

    @property
    def globals( self ):
        return self._globals

    @property
    def locals( self ):
        return self._locals

    def __init__( self, actionType = None, triggerType = None, varLookup = None, globals = None, locals = None ):
        varLookup = varLookup or (lambda i : i)

        self._locals  = dict()                      if (locals  is None) else locals
        self._globals = dict( __builtins__ = None ) if (globals is None) else globals
        self._globals['__lookup__'] = varLookup

        # Derive given classes to make own ones for the newly created ActionContext

        #---------------------------------------------
        class Action(actionType or self.Action):
        #---------------------------------------------
            _varLookup = varLookup
            _instances = []

        #---------------------------------------------
        class Trigger(triggerType or self.Trigger):
        #---------------------------------------------
            _globals = self._globals
            _locals  = self._locals

        self.lookup  = varLookup
        self.trigger = Trigger
        self.action  = Action


    def __call__( self, *args, **kwArgs ):
        return self.create( *args, **kwArgs )


    def execute( self, code ):
        exec( code, self._globals, self._locals )

