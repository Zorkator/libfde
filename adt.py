
import ctypes
from LazyException import LazyException


for f in ('libadt.gfortran.debug.32.so', 'libadt.ifort.debug.32.so'):
  try   : _libHandle = ctypes.CDLL(f); break
  except: pass
else:
  LazyException().fire()



class Meta(type(ctypes.Structure)):

  def __new__( _class, name, bases, members ):
    try:
      size = getattr( _libHandle, '{}_object_size_'.format( name.lower() ) )()
      members.setdefault( '_fields_', [('_data', ctypes.c_int8 * size)] )
    except:
      pass
    return super(Meta, _class).__new__( _class, name, bases, members )



class Object(ctypes.Structure):
  __metaclass__ = Meta
  __mangle_by__ = ['{classId}_{attrId}_']

  def __getattr__( self, name ):
    for fmt in self.__mangle_by__:
      attribId = fmt.format( classId = self.__class__.__name__.lower(), attrId = name )
      try   : attr = getattr( _libHandle, attribId ); break
      except: pass
    else:
      LazyException().fire()
    setattr( self, name, attr )
    return attr

