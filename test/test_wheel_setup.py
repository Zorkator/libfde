import unittest

#-------------------------------------------
class MyTestCase( unittest.TestCase ):
#-------------------------------------------

    @staticmethod
    def loadError( lib ):
        print( 'failed loading ' + lib )

    #         . o O (make this test running first)
    def test_0_load( self ):
        from fde.tools import core_loader as cl
        from pathlib   import Path
        cl.set( logLevel='DEBUG', onLoadError=self.loadError, footprint=True )
        print( cl.handle )
        hdlPath = Path( cl.handle._name )
        self.assertTrue( hdlPath.name in [Path(l).name for l in cl.processLibs( cl.opt('libPattern'))] )
        # testing on footprint needs initial handle loaded with footprint enabled!
        self.assertTrue( hdlPath.name in [Path(l).name for l in cl.handle.footprint] )
        self.assertTrue( cl.handle.footprint.issubset( cl.processLibs() ) )

    def test_simple( self ):
        from fde import core
        d = dict( a=1, b="test", c=1.34 )
        s = core.Scope( **d )
        print( f'\n{s}' )
        self.assertTrue( all( s[k].value == d[k] for k in d ) )
