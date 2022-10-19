
from os import path

#----------------------------------
class OptionProcessor( object ):
#----------------------------------

    __conv__ = dict()
    __opts__ = dict( debug     = 0
                   , help      = False
                   , verbosity = 1
                   , version   = False
                   )

    @property
    def opts( self ):
        "return options object."
        return self._opts


    @staticmethod
    def realpath( p ):
        "return realpath of `p` with any '~' or '~user' replaced by the user's home directory."
        return path.realpath( path.expanduser( p.strip() ) )


    @staticmethod
    def import_def( specifier, package = None ):
        """return defined symbol given by `specifier` of format [package.][module.][defId].
        It uses import_module to import the containing package/module, with `package` as
          the anchor for resolving relative imports.
        """
        from importlib import import_module
        modId, defId = specifier.rpartition('.')[::2]
        return getattr( import_module( modId, package ), defId )


    @staticmethod
    def _pickOpt( d, opt, default ):
        """Extract options `optId`, --`optId` from dictionary d.
        return value by priority: 1) dashed, 2) explicit, 3) default
        """
        null   = []
        values = [ v for v in (d.pop(k, null) for k in ('--'+opt, opt)) if v is not null ]
        return (values + [default])[0]


    @staticmethod
    def resolveEnv( envStr, maxdepth = 5, catched = (TypeError,) ):
        """returns `envStr` with environment variables expanded up to `maxdepth`.
        Exceptions given in `catched` will be ignored.
        """
        catched = catched or ( type("NullException", (Exception,), {}), )
        try:
            for i in range( maxdepth ):
                envStr, old = path.expandvars( envStr ), envStr
                if envStr == old:
                    break
        except catched:
            pass
        return envStr


    @classmethod
    def _merge_class_attrib( _class, attrId ):
        """merge option dictionaries of class hierarchy."""
        attr = dict()
        for _c in reversed( _class.mro() ):
            attr.update( getattr( _c, attrId, {} ) )
        return attr


    @classmethod
    def knownOptions( _class, optsMap = '__opts__' ):
        """return dictionary of options and default values known by this class.
        Use the class attribute set by optsMap to build up the option dictionary.
        """
        try   : return _class._knownOpts
        except:
            _class._knownOpts = _class._merge_class_attrib( optsMap )
            return _class._knownOpts


    @classmethod
    def extractOpts( _class, opts, optsMap = '__opts__', convMap = '__conv__' ):
        """extract known options from given dictionary opts.
        Use the class attribute set by optsMap to build up the option dictionary.

        """
        conv = _class._merge_class_attrib( convMap )

        for optId, valDefault in _class.knownOptions( optsMap ).items():
            optVal = _class._pickOpt( opts, optId, valDefault )
            optVal = _class.resolveEnv( optVal )

            if isinstance( optVal, Exception ): raise optVal
            else                              : yield optId, conv.get( optId, type(valDefault) )( optVal )


    def __init__( self, argDict = {}, **kwArgs ):
        """prepares instance by accepting and converting given arguments in the options object.
        Options get accepted if they are included in the class' __opts__ dict.
        Each option given in __opts__ is also recognized if it appears 'dashed', i.e. preceeded by '--'.

         * All accepted 'dashed' arguments, as usually provided by the commadline, get stored undashed with precedence
             to their undashed versions that might been given too. This is to give commandline options priority.
         * Any not recognized are left alone.

        argDict          : known options get removed from given dict.
        keyword arguments: known options get stored and might update values specified by argDict.
        """
        from ._helper import TypeObject
        self._opts = TypeObject()
        # extract known options to attributes (at least once to set defaults) ...
        argDict and self._opts.__dict__.update( self.extractOpts( argDict ) ) #< if given, from argDict
        self._opts.__dict__.update( self.extractOpts( kwArgs ) )              #< might be overridden by kwArgs!
