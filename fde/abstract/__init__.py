
__author__      = 'Josef Scheuer'
__versioninfo__ = (2, 8, 2)
__version__     = '.'.join( map( str, __versioninfo__ ) )
__all__         = "Object TypedObject TypeInfo String Item Ref List HashMap Scope".split()

for classId in __all__:
    globals()[classId] = type( classId, (object,), {} )
