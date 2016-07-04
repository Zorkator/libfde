
import os, sys, errno, traceback
from . import LibLoader, adt_loader


######################################
class NullHandle(object):
######################################
  def __null_method( self, *args, **kwArgs ):
    pass

  def __getattr__( self, name ):
    setattr( self, name, type(self).__null_method )
    return self.__null_method


######################################
class HandleWallet(object):
######################################
  pass


######################################
class Simulator(object):
######################################
  _statePath = '{classId}'
  _hooksPath = '{classId}/hooks'
  _childId   = 0

  @classmethod
  def _openFile( _class, ident = None, *args ):
    if ident is None: return NullHandle()
    try             : return (NullHandle(), sys.stdout, sys.stderr)[ident]
    except TypeError: return open( ident, *args )
    
  @classmethod
  def _makedirs( _class, path ):
    "_makedirs( pathString ) -- create directories of path recursively"
    try  : os.makedirs( path )
    except OSError as exc:
      if exc.errno != errno.EEXIST or not os.path.isdir( path ):
        raise

  @classmethod
  def write( _class, msg ):
    sys.stdout.write( "%s %d: %s" % (_class.__name__, os.getpid(), msg) )

  @classmethod
  def say( _class, msg ):
    _class.write( msg + '\n' )
    sys.stdout.flush()


  @property
  def about( self ):
    try   : return self._stock._about
    except:
      self._stock._about = dict( pid     = os.getpid()
                                , id      = self._childId
                                , classId = type(self).__name__ )
      return self._stock._about

  @property
  def handle( self ):
    try   : return self._stock._handle
    except:
      self._stock._handle = LibLoader( filePath=self.libFile, prioPathEnv=self.libPathVar ).handle
      return self._stock._handle

  @property
  def state( self ):
    try   : return self._stock._state
    except:
      from adt.core import Scope
      path = self._statePath.format( **self.about ).split('/')
      self._stock._state = Scope.getProcessScope( *path )
      return self._stock._state

  @property
  def hooks( self ):
    try   : return self._stock._hooks
    except:
      from adt.core import Scope
      path = self._hooksPath.format( **self.about ).split('/')
      self._stock._hooks = Scope.getProcessScope( *path )
      return self._stock._hooks

  @property
  def logger( self ):
    try   : return self._stock._logger
    except:
      self._stock._logger = self._openFile( self.logFileName.format( **self.about ), 'w+', self.logBuffering )
      return self._stock._logger


  @classmethod
  def extractOpts( _class, opts ):
    return dict( logFileName  = opts.pop('--logFile', "{classId}.{pid}.log")
               , workdir      = opts.pop('--workdir', '')
               , args         = opts.pop('--args', '')
               , libPathVar   = opts.pop('--libEnv', 'ADTPATH')
               , logBuffering = int(opts.pop('--logBuff', -1))
               , libFile      = os.path.abspath( opts.pop('--lib') )
               )
    

  def __opts__( self, opts ):
    opts.update( self.extractOpts( opts ) )


  def __init__( self, libName = None, **kwArgs ):
    kwArgs.setdefault( '--lib', libName )
    self.__opts__( kwArgs )
    self.__dict__.update( kwArgs )
    self._stock = HandleWallet()


  def __getstate__( self ):
    self._childId += 1
    clone = self.__dict__.copy()
    clone['_stock'] = HandleWallet()
    return clone


  def __setstate__( self, d ):
    self.__dict__.update( d )


  def log( self, msg ):
    self.logger.write( str(msg) + '\n' )


  def start( self, **kwArgs ):
    # update object members ...
    for k in set(self.__dict__).intersection( set(kwArgs) ):
      setattr( self, k, kwArgs.pop(k) )

    # create and change to working directory of simulation ...
    workdir = self.workdir.format( **self.about )
    prevdir = os.getcwd()
    if workdir:
      self._makedirs( workdir )
      os.chdir( workdir )

    # initialize simulator ...
    self.initialize( **kwArgs )
    hooks = self.hooks
    for h in hooks.keys():
      try   : hooks.setCallback( h, getattr( self, h ) )
      except: pass

    self._childId = 0
    returnCode = self.run( **kwArgs )                   #< and start it by calling run
    self.finalize( **dict( kwArgs, code=returnCode ) )  #< call finalize for possible cleanup
    os.chdir( prevdir )                                 #< restore previous directory


  def _start( self, kwArgs=dict() ):
    self.start( **kwArgs )


  def fork( self, **kwArgs ):
    from multiprocessing import Process
    childProc = Process( target=self._start, args=(kwArgs,) )
    childProc.start()
    return childProc 

  
  # override the following methods to adjust calls to simulator routines
  #   for initialization, run and finalization
  #

  def initialize( self, **kwArgs ):
    from ctypes import c_char, c_size_t, byref
    adtFilePath = (c_char * 1024)()
    self.handle.initialize_c_( byref(adtFilePath), c_size_t(len(adtFilePath)) )
    adtFilePath = adtFilePath.value.strip()
    if not adtFilePath:
      sys.stderr.write( """
        WARNING: loaded simulator doesn't report it's libadt, what might lead to inconsistent data scopes!
                 Make sure there is only one libadt to load!
                        """ )
    adt_loader.set( filePath=adtFilePath )

    # install exception hook if simulator core suports it ...
    if hasattr( self.handle, 'throw_c_' ):
      sys.excepthook = self.__except__


  def __except__( self, *args ):
    from ctypes import c_int32, c_char_p, c_size_t, byref
    code = c_int32(int('0x02200000', 16))
    what = ''.join( traceback.format_exception( *args ) )
    self.handle.throw_c_( byref(code), c_char_p(what), c_size_t(len(what)) )


  def run( self, **kwArgs ):
    try  : self.handle.run_c_()
    except AttributeError:
      raise NotImplementedError('missing implementation of run method!')


  def finalize( self, **kwArgs ):
    try  : self.handle.finalize_c_()
    except AttributeError: pass


