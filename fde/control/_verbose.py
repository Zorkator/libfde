
from ..tools import openFile, sys_channel, cached_property

#----------------------------
class Verbose( object ):
#----------------------------
    """Mixin class extending Controllable types.

    Verbose provides a basic interface for log messages put to console or logfile.
    """

    __opts__ = dict( logFile  = '{rootId}.{pid}.log'
                   , logBuff  = '-1'
                   , preamble = '{rootId} {pid}: '
                   )

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
            self.write( self.opts.preamble.format( **self.about ) + msg + '\n', channel ).flush()
        return self


    @cached_property
    def logger( self ):
        """returns lazy-opened output file handle, specified by options logFile and logBuff."""
        return openFile( self.opts.logFile.format( **self.about ), 'w+', int( self.opts.logBuff ) )


    def log( self, msg ):
        """write message string msg to logger file handle."""
        self.logger.write( str(msg) + '\n' )
        return self
