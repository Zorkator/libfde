
from ..tools            import openFile, sys_channel
from ._nativeController import cached_property

#----------------------------
class Verbose( object ):
#----------------------------
    """Mixin class extending NativeController types.

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


    def say( self, msg, channel = 1 ):
        """write and flush message string msg to system channel {1,2} => (stdout, stderr).
        Returns self.

        """
        self.write( self._preamble.format( **self.about ) + msg + '\n', channel ).flush()
        return self


    @cached_property
    def logger( self ):
        """returns lazy-opened output file handle, specified by options logFile and logBuff."""
        return openFile( self._logFile.format( **self.about ), 'w+', int( self._logBuff ) )


    def log( self, msg ):
        """write message string msg to logger file handle."""
        self.logger.write( str(msg) + '\n' )
        return self
