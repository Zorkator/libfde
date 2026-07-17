import unittest

#--------------------------------------------
class Basic( unittest.TestCase ):
#--------------------------------------------

    @staticmethod
    def _loadError( lib ):
        print( 'failed loading ' + lib )

    # tests 0-3 shouldn't trigger core loading!

    def test_0_abstract( self ):
        from fde.abstract import mixin, MixinType, abstractmethod
        from fde.abstract import Object, TypedObject, TypeInfo, String, Item, Ref, List, HashMap, Scope

    def test_1_control( self ):
        from fde.control import Controllable, FDEControllable, PyControllable
        from fde.control import ExceptionRouter, Stateful, Hookable, connect_to_hook, Verbose
        from fde.control import Startable, FDEStartable, PyStartable
        from fde.control import BaseCommandProcessor, StateCommandProcessor, Ticked
        from fde.control import Simulator, FDESimulator, PySimulator
        from fde.control import Expression
        from fde.control import ActionContextHost, ActionContext, Trigger, Action
        from fde.control import Variable

    def test_2_tools( self ):
        from fde.tools import NullHandle, Wallet, TypeObject, mkTypeObject, NullGuard, _arg, auto_raise, _decorate
        from fde.tools import LibLoader, core_loader, CDLL_t, extendLoadPaths, libPattern
        from fde.tools import dict2obj
        from fde.tools import sys_channel, openFile, makedirs
        from fde.tools import OptionProcessor
        from fde.tools import ObjectFactory, NamedObjectFactory
        from fde.tools import WeakList
        from fde.tools import Caching, cached_property

    def test_3_system( self ):
        from fde.system import Simulator

    #         . o O (make sure this test triggers the initial core loading!)
    def test_4_tools_coreLoader( self ):
        from fde.tools import core_loader as cl
        from pathlib   import Path
        cl.set( logLevel='DEBUG', onLoadError=self._loadError, footprint=True )
        print( cl.handle )
        # testing on footprint needs initial handle loaded with footprint enabled!
        from pprint import pprint as pp
        pp( ("cl.handle.filepath:", str(cl.handle.filepath)) )
        pp( ("cl.handle.footprint:", cl.handle.footprint) )
        # CAUTION: on virtual containers (e.g. CI) the map of processLibs uses paths of container mapping,
        #          which we can not compare literally with cl.handle.filepath!!
        # self.assertTrue( str(cl.handle.filepath) in cl.handle.footprint )                    #< fails on CI
        # self.assertTrue( str(cl.handle.filepath) in cl.processLibs( cl.opt('libPattern') ) ) #< fails on CI
        self.assertTrue( cl.handle.filepath.name in [Path(l).name for l in cl.handle.footprint] )
        self.assertTrue( cl.handle.filepath.name in [Path(l).name for l in cl.processLibs( cl.opt( 'libPattern' ) )] )
        self.assertTrue( cl.handle.footprint.issubset( cl.processLibs() ) )

    #         . o O (make sure this test runs AFTER testing the coreLoader!)
    def test_5_core( self ):
        from fde      import abstract
        from fde.core import Complex8, String, Item, List, Ref, Scope
        from ctypes   import POINTER, c_void_p, cast

        # Complex
        c = Complex8(1,-2)

        # String
        s = String('testinger')
        self.assertEqual( len(s), 9 )
        self.assertEqual( s[:], 'testinger' )
        self.assertTrue( isinstance(s, abstract.String))

        # Item
        i = Item()
        i = Item(1)
        i = Item(1.5)
        i = Item(complex(1,2))
        i = Item('bla & text')
        self.assertEqual( i.value, 'bla & text' )
        i.value = 42
        self.assertEqual( str(i.ftype.baseType), 'integer*4' )
        self.assertEqual( i.value, 42 )
        i.value = s
        self.assertEqual( i.value, 'testinger' )
        self.assertEqual( type(i.value), str )
        ptr = cast( id(s), POINTER(c_void_p) )
        i.value = ptr
        self.assertEqual( str(i.value), 'ptr(c_void_p)' )
        i.value = Item()
        i.value = cast( id(s), POINTER(c_void_p) )
        i.value = None
        i.value = Ref()
        self.assertEqual( str(i.value), 'ref(None)' )
        self.assertTrue( isinstance(i, abstract.Item))

        # List
        l = List()
        l.value = List()
        self.assertTrue( isinstance(l, abstract.List))

        # Scope
        d = dict( a=1, b="test", c=1.34 )
        s = Scope( **d )
        print( f'\n{s}' )
        self.assertTrue( all( s[k].value == d[k] for k in d ) )
        self.assertTrue( isinstance(s, abstract.Scope))
