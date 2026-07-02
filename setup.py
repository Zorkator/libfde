
# This package ships a native FDE shared library (.dll/.so) that is loaded
# dynamically by ctypes at runtime, but it is not a Python extension module.

from setuptools                     import setup, Distribution
from setuptools.command.build_py    import build_py
from setuptools.command.bdist_wheel import bdist_wheel

#-----------------------------------------
class BinaryDistribution(Distribution):
#-----------------------------------------
    """
    Forces setuptools to treat the wheel as platform-specific (platlib) rather than a pure-Python wheel.
    This is required because auditwheel rejects shared libraries contained in purelib wheels.
    """
    def has_ext_modules(self):
        return True

#-----------------------------------------
class BuildPY(build_py):
#-----------------------------------------
    """
    Locates the native library using the fde.tools.LibLoader, considering environment variables FDEPATH or LIBFDE
    and copies it into the wheel staging area so that it is included in the package.
    """

    def run( self ):
        import sys
        from importlib import import_module
        from pathlib   import Path
        from shutil    import copy2

        try:
            super().run()
            sys.path.insert( 0, str( Path( __file__ ).parent ) )
            core = import_module( 'fde.tools' ).core_loader.set( matchExisting=False ).handle.filepath
        except OSError as exc:
            raise RuntimeError(
                "Unable to locate the libfde shared library while building the "
                "Python package. Build the native CMake target first and expose "
                "the package binary directory via FDEPATH or LIBFDE."
            ) from exc

        (pkg := Path(self.build_lib,"fde")).mkdir( parents=True, exist_ok=True )
        self.announce( f"copying native library {core} -> {pkg}", level=2 )
        copy2( core, pkg )

#-----------------------------------------
class BDistWheel(bdist_wheel):
#-----------------------------------------
    """
    Overrides the wheel tags to "py3-none-<platform>".
    The package contains native binaries but has no dependency on the CPython ABI, so tags such as
      "cp311-cp311-<platform>"
    would be unnecessarily restrictive.
    """
    def get_tag( self ):
        _, _, platform = super().get_tag()
        return 'py3', 'none', platform


setup( distclass=BinaryDistribution,
       cmdclass={
          'build_py'   : BuildPY,
          'bdist_wheel': BDistWheel
       }
)
