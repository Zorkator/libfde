import unittest

#--------------------------------------------
class Basic( unittest.TestCase ):
#--------------------------------------------

    def test_python_access( self ):
        from ctypes import POINTER, c_void_p, cast
        import fde
        from fde.core import Complex8, String, Item, List, Ref
        #%%
        s = String('testinger')
        print("'{0}' has length {1}".format(s, len(s)))
        #%%
        c = Complex8(1,-2)
        i = Item()
        i = Item(1)
        i = Item(1.5)
        i = Item(complex(1,2))
        i = Item('bla & text')
        print( i.value )
        i.value = 42
        print( i.ftype.baseType )
        print( i.value )
        i.value = s
        print( i.value )
        len(i.value)
        #%%
        s = String('bla1')
        s = String('bla2')
        s = String('bla3')
        s = String('bla4')
        s = String('bla5')
        #%%

        l = List()
        l.value = List()
        #%%
        ptr = cast( id(s), POINTER(c_void_p) )
        i.value = ptr
        print( i.value )
        i.value = Item()
        i.value = cast( id(s), POINTER(c_void_p) )
        i.value = None
        i.value = Ref()
        print( i.value )

