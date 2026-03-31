
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 8)
__version__     = '.'.join( map( str, __versioninfo__ ) )

from ._ftypes      import Complex8, Complex16, Complex32, MemoryRef, CALLBACK
from ._typeinfo    import TypeInfo
from ._string      import String,  StringPtr
from ._item        import Item,    ItemPtr
from ._ref         import Ref,     RefPtr
from ._list        import List,    ListPtr
from ._hashmap     import HashMap, HashMapPtr
from ._scope       import Scope,   ScopePtr
