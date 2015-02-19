from distutils.core import setup
from distutils.sysconfig import get_python_lib
# cannot use setuptools, because it includes every file in svn :-(

import sys
import platform

__version__ = ""  # get rid of warning, really import it __init__.py
__author__ = ""

# read version and author from __init__.py file ignoring a possible
# exception if libadt cannot be loaded during package creation
try:
    exec(open('./adt/__init__.py').read())
except OSError:
    pass
except:
    print "Unexpected error:", sys.exc_info()[0]
    raise


def library_files():
    # TODO: Check during packaging which versions of DLL/so are available
    if platform.system() == "Windows":
        files = ['./lib/Release.x64/libadt.0.Release.x64.dll',
                 './lib/Release.Win32/libadt.0.Release.Win32.dll']
    else:
        files = ['./lib/ifort.32/libadt.0.ifort.release.32.so',
                 './lib/ifort.64/libadt.0.ifort.release.64.so']

    return files

__install_data_path__ = get_python_lib()

if len(sys.argv) > 1 and sys.argv[1] == 'bdist_wininst':
    __install_data_path__ = '..\\PURELIB\\'


setup(
    name='adt-' + platform.system(),
    version=__version__,
    author=__author__,
    author_email="",
    packages=['adt'],
    data_files=[(__install_data_path__, library_files())],
    description='Python wrapper for hash map access to CDLL shared library',
    classifiers=[
        "Programming Language :: Python",
        "Programming Language :: Python :: 2.7",
        "Operating System :: OS Independent",
        "Development Status :: 4 - Beta",
        "Environment :: Console",
    ],
)
