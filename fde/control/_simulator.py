
from . import Startable, Hookable, Stateful, ExceptionRouter, FDEController

#----------------------------------------------------------------------------------
class Simulator(Startable, Hookable, Stateful, ExceptionRouter, FDEController):
#----------------------------------------------------------------------------------
    """Convenience class preparing use of FDEController and mixin types useful for simulator codes.
    """
    __hookAlias__ = dict( NewSimulation      = None
                        , SimulationDone     = None
                        , PluginsActivated   = None
                        , NewInput           = None
                        , InputDone          = None
                        , SteadyStateDone    = None
                        , NewTimeStep        = None
                        , ProcessControlDone = None
                        , ODESolverDone      = None
                        , PlotDataReady      = None
                        , WritePlotData      = None
                        , RestartWritten     = None
                        , SevereError        = None
                        )

