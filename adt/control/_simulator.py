
from . import Startable, Hookable, Stateful, ExceptionRouter, FDEController

###################################################################################
class Simulator(Startable, Hookable, Stateful, ExceptionRouter, FDEController):
###################################################################################
  """Convenience class preparing use of FDEController and mixin types useful for simulator codes.

  """
  pass

