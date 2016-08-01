
from . import Startable, Hookable, Stateful, ExceptionRouter, ADTController

###################################################################################
class Simulator(Startable, Hookable, Stateful, ExceptionRouter, ADTController):
###################################################################################
  """Convenience class preparing use of ADTController and mixin types useful for simulator codes.

  """
  pass

