# -*- coding: utf-8 -*-
"""
Created on Sat Dec 13 00:16:33 2014

@author: zapp
"""

import os, sys, operator
import fde
from ctypes import *
from extensions import Dict

class Simulator(object):
  
  def __init__( self, soname, *args, **kwArgs ):
    self._hdl = fde.LibLoader( soname ).hdl
    self._hdl.initialize_c_()
    self._state = fde.Scope.getProcessScope()['test_simulator']
    self._hooks = self._state['hooks']
    self._hooks.setCallback( 'start',  self.start )
    self._hooks.setCallback( 'step',   self.step )
    self._hooks.setCallback( 'finish', self.finish )
    
  def run( self ):
    self._hdl.run_c_()

  def start( self ):
    print "start"
    print Dict.str(self._state)

  def step( self ):
    print "step"
    print Dict.str(self._state)

  def finish( self ):
    print "finish"
    print Dict.str(self._state)
  


if __name__ == '__main__':
  import sys
  s = Simulator( *sys.argv[1:] )
  s.run()

