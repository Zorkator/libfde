# -*- coding: utf-8 -*-
"""
Created on Mon Nov 03 19:53:28 2014

@author: sjo
"""

from __future__ import print_function
import os
import sys
import glob

basePath = os.path.join( os.path.dirname(__file__), '..' )
os.environ['LIBADT'] = glob.glob( basePath+'/lib/*/libadt*.so')[0]
sys.path.insert(0, basePath)
#%%
from ctypes import *

import adt
from adt.core import *
#%%
s = String('testinger')
print("'{0}' has length {1}".format(s, len(s)))
#%%
c = Complex8(1,-2)
i = Item()
i = Item(1)
i = Item(1.5)
i = Item(complex(1,2))
i = Item('bla & text')
print( i.value )
i.value = 42
print( i.ftype.baseType )
print( i.value )
i.value = s
print( i.value )
len(i.value)
#%%
s = String('bla1')
s = String('bla2')
s = String('bla3')
s = String('bla4')
s = String('bla5')
#%%

l = List()
l.value = List()
#%%
ptr = cast( id(s), POINTER(c_void_p) )
i.value = ptr
print( i.value )
i.value = Item()
i.value = cast( id(s), POINTER(c_void_p) )
i.value = None
i.value = Ref()
print( i.value )

