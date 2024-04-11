
import sys
print( sys.version )

from fde import core
from fde.tools import core_loader as cl

print(cl.handle)

import os, psutil
from fnmatch import fnmatch
for mod in sorted( m.path for m in psutil.Process(os.getpid()).memory_maps() ):
    fmt = ("%s", "%s\t\t\t\t\t <<< MATCH!!")[fnmatch( mod, '*/'+cl.opt('libPattern') )]
    print( fmt % mod )

# simple test ...
from fde import *
s = core.Scope(a=1, b="test", c=1.34)
print(s)

