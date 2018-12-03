from distutils.core import setup
# cannot use setuptools, because it includes every file in svn :-(

import sys

__version__ = ""  # get rid of warning, really import it __init__.py
__author__ = ""

# read version and author from __init__.py file ignoring a possible
# exception if libfde cannot be loaded during package creation
try:
    exec(open('./fde/__init__.py').read())
except OSError:
    pass
except:
    print "Unexpected error:", sys.exc_info()[0]
    raise

setup(
    name='fde',
    version=__version__,
    author=__author__,
    author_email="",
    packages=['fde'],
    description='Python wrapper for hash map access to CDLL shared library',
    classifiers=[
        "Programming Language :: Python",
        "Programming Language :: Python :: 2.7",
        "Operating System :: OS Independent",
        "Development Status :: 4 - Beta",
        "Environment :: Console",
    ],
)
