

######################################
class NullHandle(object):
######################################
  def __null_method( self, *args, **kwArgs ):
    pass

  def __getattr__( self, name ):
    setattr( self, name, type(self).__null_method )
    return self.__null_method


######################################
class Wallet(object):
######################################
  def __init__( self, members = None ):
    if members is not None:
      self.__dict__.update( members )


######################################
class NullGuard(object):
######################################
  def __init__( self, *args, **kwArgs ):
    self.__dict__.update( kwArgs, args = args )

  def __enter__( self ):
    return self

  def __exit__( self, *args ):
    return 

