
import sys, os, errno
from . import NullHandle

_null = NullHandle()

def sys_channel( num ):
  return (sys.stdin, sys.stdout, sys.stderr)[num]


def out_channel( num ):
  return (_null, sys.stdout, sys.stderr)[num]


def openFile( ident = None, *args ):
  """return io-channel or file opened according to arguments"""
  if ident is None: return _null
  try             : return sys_channel( ident )
  except TypeError: return open( ident, *args )


def makedirs( path ):
  """create directories of path recursively."""
  try  : os.makedirs( path )
  except OSError as exc:
    if exc.errno != errno.EEXIST or not os.path.isdir( path ):
      raise

