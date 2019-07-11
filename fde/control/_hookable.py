
from ._nativeController import cached_property

######################################
class Hookable(object):
######################################
  """Mixin class extending FDEController types.

  Hookable provides cashed access to certain hook scope, determined by option hooksPath

  """
  __opts__ = dict( hooksPath = '{rootId}/hooks' )

  @cached_property
  def hooks( self ):
    """return hook scope, specified by option hooksPath."""
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

