
from ctypes import CDLL, Union, sizeof, Structure, c_int8
import sys
import os
import platform
import string

_libHandle = None
_libArch = (32, 64)[sys.maxsize > 2**32]
_libArchWin = ('Win32', 'x64')[sys.maxsize > 2**32]
_libNames = (
    """
    libadt.0.gfortran.debug.{arch}.so
    libadt.0.gfortran.release.{arch}.so
    libadt.0.ifort.debug.{arch}.so
    libadt.0.ifort.release.{arch}.so
    """,
    """
    libadt.0.Debug.{archWin}.dll
    libadt.0.Release.{archWin}.dll
    """)[platform.system() == "Windows"]


def search_file(filename, search_path):
    """Given a search path, find file
    http://code.activestate.com/recipes/52224-find-a-file-given-a-search-path/
    PSF License
    """
    file_found = 0
    paths = string.split(search_path, os.pathsep)
    for path in paths:
        if os.path.exists(os.path.join(path, filename)):
            file_found = 1
            break
    if file_found:
        return os.path.abspath(os.path.join(path, filename))
    else:
        return None


for soName in map(str.strip, _libNames.format(arch=_libArch,
                                              archWin=_libArchWin).split()):
    try:
        search_path = '.'
        if 'LD_LIBRARY_PATH' in os.environ:
            search_path += os.pathsep + os.environ['LD_LIBRARY_PATH']
        if 'PATH' in os.environ:
            search_path += os.pathsep + os.environ['PATH']
        find_file = search_file(str(soName), search_path)
        if find_file:
            print "Shared library found at %s" % find_file
            _libHandle = CDLL(find_file)
            break
        else:
            print "Shared library " + str(soName) + " not found"
    except Exception as e:
        print 'tried loading "%s": %s' % (soName, e)
else:
    raise OSError("unable to locate ADT's shared library")

print 'loaded "%s"' % soName


class _Meta(type(Union)):

    def __new__(cls, name, bases, members):
        from operator import add
        method = '{0}_{{0}}c_'.format(name.lower())
        members['__typeprocs__'] = list(members.get('__typeprocs__', [method])) \
            + reduce(add, (getattr(b, '__typeprocs__', []) for b in bases))

        fields = list(members.pop('_fields_', []))
        anonym = list(members.pop('_anonymous_', []))
        size = getattr(_libHandle, method.format('object_size_'), lambda: 0)()

        if fields:
            _Struct = type('_Struct', (Structure,), dict(_fields_=fields,
                                                         _anonymous_=anonym))
            size = max(size, sizeof(_Struct))
            fields = [('_struct', _Struct)]
            anonym = ['_struct']

        size and fields.append(('_data', size * c_int8))
        members.update(_fields_=fields, _anonymous_=anonym)
        return super(_Meta, cls).__new__(cls, name, bases, members)


class Compound(Union):
  __metaclass__ = _Meta
  __typeprocs__ = [] #< no procedures for Compound

  def __getattr__( self, name ):
    if name is '_needs_delete': #< if we end up here, slot _needs_delete has not been set!
      return False
    if name in ('__members__', '__methods__'):
      return {}

        for fmt in self.__typeprocs__:
            try:
                attr = getattr(_libHandle, fmt.format(name))
                break
            except:
                pass
        else:
            raise AttributeError("'%s' object has no attribute '%s'"
                                 % (self.__class__.__name__, name))
        setattr(type(self), name, attr)
        return attr
