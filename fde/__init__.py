
__author__      = 'Josef Scheuer'
__all__         = 'abstract control core system tools'.split()
__versioninfo__ = (2, 8, 8)
__version__     = '.'.join( map( str, __versioninfo__ ) )

try:
    from .__rev__ import ident as __revision__
except:
    __revision__ = __version__
