from setuptools     import setup, find_packages
from setuptools_scm import get_version
import sysconfig

def copy_sharedlib():
    from platform import system
    from os       import environ as env
    from pathlib  import Path
    from shutil   import copy2

    ptrn = ('libfde.so*', '*fde.*.dll')[system() == "Windows"]
    core = next( Path( env["FDEPATH"] ).glob( ptrn ) )
    dest = Path(__file__).parent/'fde'/core.name
    copy2( core, dest )


copy_sharedlib()
setup(
    name             = 'fde',
    description      = 'Python wrapper for FDE loading and using shared library',
    license          = "GNU Lesser General Public License, Version 3 (LGPLv3)",
    version          = get_version(),
    author           = "Josef Scheuer",
    author_email     = "sjo@grs.de",
    install_requires = ['six', 'psutil', 'fnmatch2', 'winshlex', 'click'],
    packages         = find_packages( include=['fde', 'fde.*'] ),
    package_data     = {
        'fde': ["*.dll", "*.so", "*.so.*"]
    },
    #
    options = {
        'bdist_wheel': {
            'plat_name': sysconfig.get_platform()
        }
    },
    classifiers = [
        "Development Status :: 4 - Beta",
        "Environment :: Console",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Intended Audience :: Developers",
        "Operating System :: OS Independent",
    ],
)
