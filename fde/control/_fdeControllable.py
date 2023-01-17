
from ..tools        import LibLoader, cached_property
from ._controllable import Controllable

#--------------------------------------------
class FDEControllable( Controllable ):
#--------------------------------------------
    """Control class for handling of C-compatible native code libraries, using libfde.

    FDEControllable offers only rudimentary library handling, like lazy loading and customization by options.
    However, is prepared to be extended by inheritance and mixin-classes, what allows composing control features
      according to the native code capabilities.

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
    __conv__ = dict( lib      = Controllable.realpath )
    __opts__ = dict( lib      = LookupError( 'missing library specification!' )
                   , libEnv   = 'FDEPATH'
                   , initFunc = 'initialize_c_'
                   )

    _fdeLibPathError = \
      """WARNING:
         loaded library {lib} doesn't report it's libfde.
         This might lead to inconsistent data scopes!
         Make sure there is only one libfde to load!
      """

    @cached_property
    def handle( self ):
        """lazy-load property returning handle of loaded library."""
        opts = {'--debug': self.opts.debug > 2, '--verbose': self.opts.verbosity > 2}
        return LibLoader( filePath=self.opts.lib, prioPathEnv=self.opts.libEnv, **opts ).handle


    def __initialize__( self, rootId ):
        """prepare and call initFunc (initialize_c_)."""
        # This function expects a string buffer containing the id of the created root scope.
        # The same buffer is also used to return the filePath of the loaded libfde.
        from ..tools import core_loader
        from ctypes  import create_string_buffer, byref, c_size_t
        from sys     import stderr

        infoBuff = create_string_buffer( str.encode( rootId + ' ' * (1024 - len(rootId)) ) )
        self.handle[ self.opts.initFunc ]( byref(infoBuff), c_size_t(len(infoBuff)-1) )
        fdelibPath = infoBuff.value.strip().decode( 'utf-8' )
        if fdelibPath != rootId:
            core_loader.set( filePath=fdelibPath )
        elif self.opts.verbosity > 0:
            stderr.write( self._fdeLibPathError.format( lib=self.handle._name ) )
