from setuptools import setup

import shutil
import fde
from fde.tools import core_loader

shutil.copy2( core_loader.handle._name, fde.__path__[0] )

setup(
    name='fde',
    version=fde.__version__,
    author=fde.__author__,
    author_email="",
    install_requires=['six', 'psutil', 'intel-fortran-rt'],
    packages=['fde'] + ['fde.' + mod for mod in fde.__all__],
    package_data={'fde': ["*.dll", "*.so"]},
    description='Python wrapper for FDE loading and using shared library',
    classifiers=[
        "Development Status :: 4 - Beta",
        "Environment :: Console",
        "License :: OSI Approved :: GNU Lesser General Public License, Version 3 (LGPLv3)",
        "Programming Language :: Python",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Intended Audience :: Developers",
        "Operating System :: OS Independent",
    ],
)
