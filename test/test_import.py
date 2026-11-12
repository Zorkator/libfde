import unittest

#-----------------------------------
class ImportTest( unittest.TestCase ):
#-----------------------------------

    def test_paths_and_import( self ):
        import sys
        from pprint import pprint
        pprint(sys.path)
        from fde import __version__, core, tools
        print( f'version: {__version__}' )
        print( f'core   : {core}' )
        print( f'corelib: {tools.core_loader.handle}' )
