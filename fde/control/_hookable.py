

######################################
class Hookable(object):
######################################
  """Mixin class extending FDEController types.
    
  Hookable provides cashed access to certain hook scope, determined by option hooksPath

  """
  __opts__ = dict( hooksPath = '{rootId}/hooks' )
  
  @property
  def hooks( self ):
    """return hook scope, specified by option hooksPath."""
    try   : return self._stock._hooks
    except:
      self._stock._hooks = self._get_hooks()
      return self._stock._hooks


  def _get_hooks( self ):
    from fde.core import Scope
    path = self._hooksPath.format( **self.about ).split('/')
    return Scope.getProcessScope( *path )


  @property
  def activeHookCount( self ):
    return sum( map( bool, self.hooks.values() ) )


  def initialize( self, **kwArgs ):
    super(Hookable, self).initialize( **kwArgs )

    # set implemented callback hooks ...
    hooks = self.hooks
    for h in hooks.keys():
      try   : hooks.setCallback( h, getattr( self, h ) )
      except: pass

