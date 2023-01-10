
from ._fdeStartable     import FDEStartable
from ._simulatorITF     import SimulatorITF
from ._exceptionRouter  import ExceptionRouter
from ._fdeControllable  import FDEControllable

#--------------------------------------------------------------------------------------------
class FDESimulator(FDEStartable, SimulatorITF, ExceptionRouter, FDEControllable):
#--------------------------------------------------------------------------------------------
    pass

# compatibility
Simulator = FDESimulator
