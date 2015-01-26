
from ctypes  import *
from _object import Object
from _item   import Item
from _ftypes import mappedType


@mappedType( 'hashmap', 'type(HashMap_t)' )
class HashMap(Object):

	def __len__( self ):
		return self.len_( byref(self) )

	def clear( self ):
		self.clear_( byref(self) )


	def get( self, key, default = None ):
		ptr = POINTER(Item)()
		dummy = "{}".format(ptr) # de-optimize Python (????), so next line works
		self.get_ptr_( byref(ptr), byref(self), c_char_p(key), c_int(len(key)) )
		if ptr: return ptr.contents.value
		else  : return default

