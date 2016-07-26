
import os, sys, errno, traceback
from ctypes    import c_int32, create_string_buffer, c_char_p, c_size_t, byref
from ..tools   import LibLoader, core_loader, NullHandle, Wallet


######################################
class Simulator(object):
######################################
  _rootId    = '{classId}'
  _statePath = '{rootId}'
  _hooksPath = '{rootId}/hooks'
  _childId   = 0
  _channels  = (NullHandle(), sys.stdout, sys.stderr)

  @classmethod
  def _openFile( _class, ident = None, *args ):
    if ident is None: return NullHandle()
    try             : return _class._channels[ident]
    except TypeError: return open( ident, *args )
    
  @classmethod
  def _makedirs( _class, path ):
    "_makedirs( pathString ) -- create directories of path recursively"
    try  : os.makedirs( path )
    except OSError as exc:
      if exc.errno != errno.EEXIST or not os.path.isdir( path ):
        raise

  @classmethod
  def extractOpts( _class, opts ):
    return dict( logFileName  = opts.pop('--logFile', "{rootId}.{pid}.log")
               , workdir      = opts.pop('--workdir', '')
               , args         = opts.pop('--args', '')
               , libPathVar   = opts.pop('--libEnv', 'ADTPATH')
               , logBuffering = int(opts.pop('--logBuff', -1))
               , libFile      = os.path.abspath( opts.pop('--lib') )
               , rootId       = opts.pop('--rootId', None) or _class._rootId.format( classId=_class.__name__ )
               )
    

  @classmethod
  def write( _class, msg, channel=1 ):
    chnl = _class._channels[channel]
    chnl.write( "%s %d: %s" % (_class.__name__, os.getpid(), msg) )
    return chnl

  @classmethod
  def say( _class, msg, channel=1 ):
    return _class.write( msg + '\n', channel )


  @property
  def about( self ):
    try   : return self._stock._about
    except:
      self._stock._about = dict( pid     = os.getpid()
                               , id      = self._childId
                               , rootId  = self.rootId
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


  def __opts__( self, opts ):
    opts.update( self.extractOpts( opts ) )


  def __init__( self, libName = None, **kwArgs ):
    kwArgs.setdefault( '--lib', libName )
    self.__opts__( kwArgs )
    self.__dict__.update( kwArgs )
    self._stock = Wallet()


  def __getstate__( self ):
    self._childId += 1
    clone = self.__dict__.copy()
    clone['_stock'] = Wallet()
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
    # prepare call to initialize_c_:
    #   it expects a string buffer containing the id of the created root scope.
    #   This buffer is used to return the filePath of the loaded libadt.
    #
    infoBuff = create_string_buffer( self.rootId + ' ' * (1024 - len(self.rootId)) )

    self.handle.initialize_c_( byref(infoBuff), c_size_t(len(infoBuff)-1) )

    infoBuff = infoBuff.value.strip()
    if infoBuff == self.rootId:
      self.say( """
        WARNING: loaded simulator doesn't report it's libadt, what might lead to inconsistent data scopes!
                 Make sure there is only one libadt to load!
                """ )
    core_loader.set( filePath=infoBuff )

    # set implemented callback hooks ...
    hooks = self.hooks
    for h in hooks.keys():
      try   : hooks.setCallback( h, getattr( self, h ) )
      except: pass

    # install exception hook if simulator core suports it ...
    if hasattr( self.handle, 'throw_c_' ):
      sys.excepthook = self.__except__


  def __except__( self, *args ):
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


