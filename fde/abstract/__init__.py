
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 7)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = "Object TypedObject TypeInfo String Item Ref List HashMap Scope".split()

for classId in __all__:
    globals()[classId] = type( classId, (object,), {} )

from ._coopMixin import MixinType, mixin, abstractmethod

__all__.extend( 'MixinType mixin'.split() )
