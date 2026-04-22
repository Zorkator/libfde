
from ..tools        import openFile, sys_channel, cached_property
from ._controllable import Controllable
from functools      import wraps


@Controllable.mixin
#----------------------------
class Verbose( object ):
#----------------------------
    """Mixin class extending Controllable types.

    Verbose provides a basic interface for log messages put to console or logfile.
    """

    __opts__ = dict( logFile   = '{rootId}.{pid}.log'
                   , logBuff   = 1
                   , logFormat = '{rootId} {pid}: {0}\n'
                   )
    __info__ = dict( logFile   = 'logfile for logging verbose output'
                   , logBuff   = 'buffer for logging verbose output'
                   , logFormat = 'format for log messages'
                   )

    def __init__( self, *args, **kwArgs ):
        from pprint import pformat
        super(Verbose, self).__init__( *args, **kwArgs )
        self.say( 2, lambda: '\n' + pformat( self.opts.__getstate__(), indent=3 ) )

    def write( self, msg, channel = 1 ):
        """write message string msg to system channel {1,2} => (stdout, stderr).
        Returned channel might be used to trigger flush().

        """
        chnl = sys_channel( channel )
        chnl.write( msg )
        return chnl


    def say( self, verbosity, msg, channel = 1 ):
        """if verbosity level met, write and flush message string msg to system channel {1,2} => (stdout, stderr).
        Returns self.

        """
        if self.opts.verbosity >= verbosity:
            msg = msg() if callable(msg) else msg
            self.write( self.opts.logFormat.format( msg, **self.about ), channel ).flush()
        return self


    @cached_property
    def logger( self ):
        """returns lazy-opened output file handle, specified by options logFile and logBuff."""
        return openFile( self.opts.logFile.format( **self.about ), 'w+', self.opts.logBuff )


    def log( self, msg ):
        """write message string msg to logger file handle."""
        self.logger.write( str(msg) + '\n' )
        return self


    @wraps(print)
    def print( self, *args, **kwArgs ):
        # make print overridable
        return print( *args, **kwArgs )
