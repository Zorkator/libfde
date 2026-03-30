import unittest

#-------------------------------------------
class MyTestCase( unittest.TestCase ):
#-------------------------------------------

    @staticmethod
    def loadError( lib ):
        import os
        try   : os.system( 'ldd ' + lib )
        except: print( 'failed loading ' + lib )

    def test_load( self ):
        from fde.tools import core_loader as cl
        cl.set( logLevel='DEBUG', onLoadError=self.loadError, footprint=True )
        print( cl.handle )
        self.assertTrue( cl.handle._name in cl.processLibs( cl.opt('libPattern') ) )
        self.assertTrue( cl.handle._name in cl.handle.footprint )
        self.assertTrue( cl.handle.footprint.issubset( cl.processLibs() ) )


    def test_simple( self ):
        from fde import core
        d = dict( a=1, b="test", c=1.34 )
        s = core.Scope( **d )
        print( f'\n{s}' )
        self.assertTrue( all( s[k].value == d[k] for k in d ) )
