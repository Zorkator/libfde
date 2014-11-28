# -*- coding: utf-8 -*-
"""
Created on Mon Nov 03 19:53:28 2014

@author: sjo
"""

import sys
import os
from ctypes import *

try:
	os.chdir('C:\\Users\\sjo\\Documents\\development\\zappralot\\libadt\\vc2010\\exe')
	sys.path.insert(0, 'C:\\Users\\sjo\\Documents\\development\\zappralot\\libadt')
except:
	pass

import adt

s = adt.String('testinger')
print "'{0}' has length {1}".format(s, len(s))

c = adt.Complex8(1,-2)
i = adt.Item()
i = adt.Item(1)
i = adt.Item(1.5)
i = adt.Item(complex(1,2))
i = adt.Item('bla & text')
print i.value
i.value = 42
print i.ftype.baseType
print i.value
i.value = s
print i.value
len(i.value)

s = adt.String('bla1')
s = adt.String('bla2')
s = adt.String('bla3')
s = adt.String('bla4')
s = adt.String('bla5')


l = adt.List()
l.value = adt.List()

ptr = cast( id(s), POINTER(c_void_p) )
i.value = ptr
print i.value
i.value = adt.Item()
i.value = cast( id(s), POINTER(c_int32) )
i.value = None
i.value = adt.Ref()
print i.value

