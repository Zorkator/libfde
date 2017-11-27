

######################################
class Stateful(object):
######################################
  """Mixin class extending ADTController types.
    
  Stateful provides cashed access to certain state scope, determined by option statePath

  """
  __opts__   = dict( statePath = '{rootId}' )

  @property
  def scopeKeyOperator( self ):
    return getattr( self._stock, '__keyop__', None )


  @scopeKeyOperator.setter
  def scopeKeyOperator( self, op ):
    self._stock.__keyop__ = op


  @property
  def state( self ):
    """return state scope, specified by option statePath."""
    try   : return self._stock._state
    except:
      self._stock._state = self._get_state()
      return self._stock._state


  def _get_state( self ):
    from adt.core import Scope
    path = self._statePath.format( **self.about ).split('/')
    return Scope.getProcessScope( *path )


  def setStateData( self, dataDict ):
    self.state.updateDomain( dataDict, self.scopeKeyOperator )
    return True


  def getStateData( self, keyList = [] ):
    if keyList:
      pairs = zip( *self.state.iterDomain( keyList, self.scopeKeyOperator ) )
      self._stock._stateData = pairs and pairs[1] or []
    return getattr( self._stock, '_stateData', [] )



from functools import wraps

######################################
def cached_property( f ):
######################################
  @wraps(f)
  def _wrapper( self ):
    try  : return getattr( self._stock, '_p_' + f.func_name )
    except AttributeError:
      val = f( self )
      setattr( self._stock, '_p_' + f.func_name, val )
      return val
  return property( _wrapper )

