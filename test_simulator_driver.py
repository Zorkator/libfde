# -*- coding: utf-8 -*-
"""
Created on Sat Dec 13 00:16:33 2014

@author: zapp
"""

import os, sys, operator
import adt
from ctypes import *

class Simulator(object):
  
  def __init__( self, soname ):
    self._hdl = CDLL( soname )
    
    state, hooks = POINTER(adt.HashMap)(), POINTER(adt.HashMap)()
    self._hdl.get_maps_( byref(state), byref(hooks) )
    self._state = state.contents
    self._hooks = hooks.contents
    self._hdl.init_simulator_()
    self._cbs = dict()
    #ids, idtab, names = self['id_array'], self['id_table'], self['name_array']
    #self._id_array    = ids.castTo( (c_char * 10) * ids.shape[0] )
    #self._id_table    = idtab.castTo( (c_char * 5) * reduce( operator.mul, idtab.shape ) )
    #self._name_array  = names.castTo( (c_char * 48) * names.shape[0] )
    
  def __getitem__( self, ident ):
    return self._state.get(ident).value.contents
    
  def __setitem__( self, ident, val ):
    ref = self._state.get(ident).value
    if ref.rank: ref.contents[:]    = val
    else       : ref.contents.value = val
    
  def run( self ):
    self._hdl.run_simulation_()
    
  def setCallback( self, cbId, func ):
    if type(type(func)) is not type(adt.CALLBACK):
      self._cbs[cbId] = func = adt.CALLBACK(func)
    return self._hdl.set_callback_( c_char_p(cbId), func, c_int(len(cbId)) )

s = Simulator('exe/libtestsim_so.debug.64.gfortran.so')

def step_cb():
  print s['t']

s.setCallback( 'step', step_cb )
#for i in s._id_array:
#  print i[:]
#for i in s._name_array:
#  print i[:]
#s.run()
#
#for i in s['string_array']:
#  i.value = "dynamic string %s" % repr(i)

#s['t'] = 0
#s['dt'] = 0.001
s.run()

