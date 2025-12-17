# -*- coding: utf-8 -*-
"""
Created on Sat Dec 13 00:16:33 2014

@author: zapp
"""

# TODO: needs update!

from fde.tools import LibLoader
from fde.core  import Scope
from pprint    import pprint

class Simulator(object):

  def __init__( self, soname, *args, **kwArgs ):
    self._hdl = LibLoader( soname ).handle
    self._hdl.initialize_c_()
    self._state = Scope.getProcessScope()['test_simulator']
    self._hooks = self._state['hooks']
    self._hooks.setCallback( 'start',  self.start )
    self._hooks.setCallback( 'step',   self.step )
    self._hooks.setCallback( 'finish', self.finish )

  def run( self ):
    self._hdl.run_c_()

  def start( self ):
    print( "start" )
    pprint( self._state )

  def step( self ):
    print( "step" )
    pprint( self._state )

  def finish( self ):
    print( "finish" )
    pprint( self._state )



if __name__ == '__main__':
  import sys
  s = Simulator( *sys.argv[1:] )
  s.run()
