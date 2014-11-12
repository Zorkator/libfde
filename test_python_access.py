# -*- coding: utf-8 -*-
"""
Created on Mon Nov 03 19:53:28 2014

@author: sjo
"""

import sys
import os
from ctypes import *
os.chdir('C:\\Users\\sjo\\Documents\\development\\zappralot\\libadt\\vc2010\\libadt_dll\\Debug')
sys.path.insert(0, 'C:\\Users\\sjo\\Documents\\development\\zappralot\\libadt')

import adt

s = adt.String('testinger')
print "'{0}' has length {1}".format(s, len(s))

c = adt.Complex_8(1,-2)
i = adt.Item('bla & text')
print i.value
i.value = 42
print i.ftype.baseType
print i.value
i.value = s
print i.value
len(i.value)


