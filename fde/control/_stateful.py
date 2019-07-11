
from ._nativeController import cached_property
from ._variable         import Variable

######################################
class Stateful(object):
######################################
  """Mixin class extending FDEController types.

  Stateful provides cashed access to certain state scope, determined by option statePath

  """
  __opts__ = dict( rootPath  = '{rootId}'
                 , statePath = '{rootId}/state'
                 )

  @classmethod
  def makeKeyTokenizer( _class, sep = None, conv = [] ):
    """create key tokenizer for given string separator sep and optional type converters conv."""
    if   sep is None: return  lambda k: k                #< null tokenizer, keep as is
    elif sep != ''  : tokOp = lambda k: tuple(filter( bool, map( type(k).strip, k.split(sep) ) ))
    else            : tokOp = lambda k: tuple(k.split()) #< for empty separator split at whitespaces

    if conv:
      # if there's at least one conversion ...
      def _convert( k ):
        for c in conv:
          try   : return c(k)
          except: pass
        return k
      return lambda k: tuple(map( _convert, tokOp(k) )) #< ... wrap tokenizer by converter
    else:
      return tokOp

  @property
  def keyTokenizer( self ):
    return getattr( self._stock, '__keyTok__', None )


  @keyTokenizer.setter
  def keyTokenizer( self, op ):
    self._stock.__keyTok__ = op


  def setKeyTokenizer( self, sep = None, conv = [] ):
    self.keyTokenizer = self.makeKeyTokenizer( sep, conv )


  @cached_property
  def root( self ):
    """return root scope, specified by option rootPath."""
    return self._get_path_scope( self._rootPath )


  @cached_property
  def state( self ):
    """return state scope, specified by option statePath."""
    return self._get_path_scope( self._statePath )


  def _get_path_scope( self, path ):
    """return nested scope specified by given path."""
    from fde.core import Scope
    pathList = path.format( **self.about ).split('/')
    return Scope.getProcessScope( *pathList )


  def setData( self, dataDict, keyTok = None ):
    """set rootPath-based data given by dataDict."""
    self.root.updateDomain( dataDict, keyTok or self.keyTokenizer )
    return True


  def getData( self, keyList = [], keyTok = None ):
    """get data references for given rootPath-based keyList.
    getData caches the result of the latest requested keyList and returns it if keyList is empty.
    """
    if keyList:
      pairs = zip( *self.root.iterDomain( keyList, keyTok or self.keyTokenizer ) )
      self._stock._req_data = pairs and pairs[1] or []
    return getattr( self._stock, '_req_data', [] )


  @cached_property
  def Var( self ):
    """return root-Scope based variable lookup object."""
    return self.makeVariableLookup()


  def makeVariableLookup( self, rootScope = None, keyTok = None, varType = Variable ):
    """return new variable factory that does <rootScope>-based path lookups and creates <varType> from the result.
    The optional argument keyTok allows specifying a special keyTokenizer different from that currently set.
    """
    from ..tools import UniqueObjectFactory
    rootScope = rootScope or self.root
    keyTok    = keyTok    or self.keyTokenizer

    def _createVar( ident, *args, **kwArgs ):
      return varType( rootScope[ident] )
    return UniqueObjectFactory( _createVar, keyTok )


  def makeActionContext( self, actionType = None, triggerType = None, varLookup = None ):
    """ """
    from . import ActionContext
    return ActionContext( actionType, triggerType, varLookup or self.Var )

