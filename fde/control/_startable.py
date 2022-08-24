
import os
from ..tools import makedirs, NullGuard, debug

#----------------------------
class Startable(object):
#----------------------------
    """Mixin class extending Controllable types.

    Startable provides the interface for starting the main process loop of native code.
    The main loop is expected to be implemented as:

    void start_c_( int32_t *returnCode, const char *commandString, size_t length )

    This call signature can be changed by overriding the __start__ method.
    After the main loop's exit, an optional finalize routine might be triggered, e.g. for cleanup.
    If such routine exists, its signature is assumed as:

    void finalize_c_( void )

    Again, this signature can be changed by overriding the __finalize__ method.

    Furthermore, by the method fork() Startable basically prepares re-starting the native code
      within another Python process.
    """

    __opts__ = dict( args    = ''
                   , workdir = ''
                   )


    def fork( self, **kwArgs ):
        """fork the current Python process and run start() method of instance."""
        from multiprocessing import Process
        if self.opts.debug > 0: debug()
        childProc = Process( target=self.start, kwargs=kwArgs )
        childProc.start()
        return childProc


    def start( self, workdir = '', args = '', **kwArgs ):
        """initializes Startable instance and call its __start__ method, passing args and kwArgs.
        If `workdir` is specified the working directory is changed before calling __start__.
        The current workdir gets restored afterwards.
        The `args` argument can be given as string of arguments or a list of string arguments.
        Environment variables get resolved before passing the argument strings to __start__.
        """
        if self.opts.debug > 0: debug()

        # create and change to working directory of simulation ...
        workdir = workdir or self.opts.workdir.format( **self.about )
        prevdir = os.getcwd()
        if workdir:
            makedirs( workdir )
            os.chdir( workdir )

        try:
            # determine argument list ... if not given explicitly use predefined
            args = args or self.opts.args
            try   : args = args.strip and [args] #< if args is string wrap it by list
            except: pass
            args = map( self.resolveEnv, args ) #< resolve environment variables in args ...

            with getattr( self, 'routedExceptions', NullGuard )(): #< mixin-method might not be available.
                code = self.__start__( *args, **kwArgs )
                code = self.finalize( code, **kwArgs )
        finally:
            os.chdir( prevdir )
        return code


    def finalize( self, code, **kwArgs ):
        return self.__finalize__( code, **kwArgs )

    # methods to be [re-]implemented by subclasses

    def __start__( self, *args, **kwArgs ):
        raise NotImplementedError

    def __finalize__( self, code, **kwArgs ):
        raise NotImplementedError
