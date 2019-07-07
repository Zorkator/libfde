
#####################################
class ObjectFactory(object):
#####################################

  def __init__( self, objCreator, identOp = None ):
    self._objCreator = objCreator
    self._identOp    = identOp or (lambda i: i)
    self._instances  = dict()


  def __call__( self, ident, *args, **kwArgs ):
    ident = self._identOp( ident )
    try   : return self._instances[ident]
    except:
      self._instances[ident] = obj = self._objCreator( ident, *args, **kwArgs )
      return obj


  def __getitem__( self, ident ):
    return self._instances[ self._identOp( ident ) ]


  def delete( self, ident ):
      try   : del self._instances[ self._identOp( ident ) ]
      except: raise KeyError("there is no %s variable with name %s" % (self._objType.__name__, ident) )

  @property
  def count( self ):
    return len(self._instances)

  @property
  def pairs( self ):
    return self._instances.items()


  @property
  def names( self ):
    return self._instances.keys()


  @property
  def vars( self ):
    return self._instances.values()


  @property
  def create( self ):
    return self


