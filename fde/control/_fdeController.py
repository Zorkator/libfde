
import sys
from ctypes             import create_string_buffer, byref, c_size_t
from ..tools            import core_loader
from ._nativeController import NativeController


#--------------------------------------------
class FDEController( NativeController ):
#--------------------------------------------
    """Control class for handling of C-compatible native code libraries, using libfde.

    The design of libfde's mechanisms for accessing data and controlling the execution flow bases
      on a hierarchy of nested Scopes (basically HashMaps), which gets accessable by a unique process scope.
    Therefore, the native code library is expected to provide an initialization routine that creates
      the library's base scope within the process scope.
    By this scope the library provides access to any data or callback hooks the native code implements.

    For the native initialization function the following signature is assumed:

    void initialize_c_( char *infoBuffer, size_t length )

    The given infoBuffer is used for two purposes:
     * it provides the initialization with the rootId it **should** use for naming its base scope.
       Note that the rootId is stored conforming to Fortran convention, without 0-terminator and filled with blanks.

     * the initialization **should** use the infoBuffer to store the filePath of the libfde it loaded.
       This filePath allows the python binding to use the very same library, what is crucial for exchanging data.
    """

    _fdeLibPathError = \
        """WARNING:
           loaded library {lib} doesn't report it's libfde.
           This might lead to inconsistent data scopes!
           Make sure there is only one libfde to load!
        """

    __opts__ = dict( rootId   = '{classId}'
                   , initFunc = 'initialize_c_'
                   )


    def _get_about( self ):
        return dict( super(FDEController, self)._get_about()
                   , rootId = self._rootId.format( classId = type(self).__name__ )
                   )


    def initialize( self, **kwArgs ):
        super(FDEController, self).initialize( **kwArgs )

        rootId     = self._rootId.format( **self.about )
        fdelibPath = self.__initialize__( rootId )
        if   fdelibPath != rootId: core_loader.set( filePath=fdelibPath )
        elif self._verbosity > 0 : sys.stderr.write( self._fdeLibPathError.format( lib=self.handle._name ) )


    def __initialize__( self, rootId ):
        """prepare and call initFunc (initialize_c_)."""
        # This function expects a string buffer containing the id of the created root scope.
        # The same buffer is also used to return the filePath of the loaded libfde.
        infoBuff = create_string_buffer( str.encode( rootId + ' ' * (1024 - len(rootId)) ) )
        self.handle[ self._initFunc ]( byref(infoBuff), c_size_t(len(infoBuff)-1) )
        return infoBuff.value.strip().decode( 'utf-8' )
