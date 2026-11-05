
global Object, TypedObject, TypeInfo, String, Item, Ref, List, HashMap, Scope

for classId in "Object TypedObject TypeInfo String Item Ref List HashMap Scope".split():
    globals()[classId] = type( classId, (object,), {} )

from ._coopMixin import MixinType, mixin, abstractmethod
