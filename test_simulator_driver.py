# -*- coding: utf-8 -*-
"""
Created on Sat Dec 13 00:16:33 2014

@author: zapp
"""

import os, sys
import adt
from ctypes import *

class Simulator(object):
  
  CALLBACK = CFUNCTYPE(None)
  
  def __init__( self ):
    self._hdl = CDLL('test_simulator.so')
    
    state, hooks = POINTER(adt.HashMap)(), POINTER(adt.HashMap)()
    self._hdl.get_maps_( byref(state), byref(hooks) )
    self._state = state.contents
    self._hooks = hooks.contents
    self._hdl.init_simulator_()
    self._cbs = dict()
    
  def __getitem__( self, ident ):
    return self._state.get(ident).contents.value
    
  def __setitem__( self, ident, val ):
    self._state.get(ident).contents.value = val
    
  def run( self ):
    self._hdl.run_simulation_()
    
  def setCallback( self, cbId, func ):
    if type(type(func)) is not type(self.CALLBACK):
      self._cbs[cbId] = func = self.CALLBACK(func)
    return self._hdl.set_callback_( c_char_p(cbId), func, c_int(len(cbId)) )

s = Simulator()

def step_cb():
  print s['t']

s.setCallback( 'step', step_cb )
s.run()
s['t'] = 0
s['dt'] = 0.001
s.run()

