
from ._hookable import Hookable
from ._stateful import Stateful

#-----------------------------------------
class SimulatorITF(Hookable, Stateful):
#-----------------------------------------
    """Convenience class providing useful interface for simulator codes.
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
