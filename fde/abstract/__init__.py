
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 8)
__version__     = '.'.join( map( str, __versioninfo__ ) )

global Object, TypedObject, TypeInfo, String, Item, Ref, List, HashMap, Scope

for classId in "Object TypedObject TypeInfo String Item Ref List HashMap Scope".split():
    globals()[classId] = type( classId, (object,), {} )

from ._coopMixin import MixinType, mixin, abstractmethod
