
from os import path

#----------------------------------
class OptionProcessor( object ):
#----------------------------------
    __conv__ = dict()
    __opts__ = dict( debug     = 0
                   , verbosity = 1
                   )

    @property
    def opts( self ):
        "return options object."
        return self._opts


    @classmethod
    def realpath( _class, p ):
        "return realpath of `p` with environment variables resolved and '~'/'~user' replaced by user's home directory."
        return path.realpath( path.expanduser( _class.resolveEnv( p.strip() ) ) )


    @classmethod
    def filepath( _class, p ):
        "return realpath of `p`"
        return _class.realpath( p )


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
    def _pickOpt( d, optId, default ):
        """Extract options `optId`, --`optId` from dictionary d.
        return value by priority: 1) dashed, 2) explicit, 3) default
        """
        null   = []
        values = [v for v in (d.pop(k, null) for k in ('--' + optId, optId)) if v is not null]
        return (values + [default])[0]


    @staticmethod
    def resolveEnv( envStr, maxdepth = 5, caught = (TypeError,) ):
        """returns `envStr` with environment variables expanded up to `maxdepth`.
        Exceptions given in `catched` will be ignored.
        """
        caught = caught or (type( "NullException", (Exception,), {} ),)
        try:
            for i in range( maxdepth ):
                envStr, old = path.expandvars( envStr ), envStr
                if envStr == old:
                    break
            else:
                raise RecursionError( 'environment variables nested too deep!', envStr )
        except caught:
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
    def _optsMap( _class, optsMap = '__opts__' ):
        try   : opts = vars(_class)['.' + optsMap]
        except:
            opts = _class._merge_class_attrib( optsMap )
            setattr(_class, '.' + optsMap, opts )
        return opts


    @classmethod
    def _convMap( _class, convMap = '__conv__' ):
        try   : conv = vars(_class)['.' + convMap]
        except:
            conv = _class._merge_class_attrib( convMap )
            setattr(_class, '.' + convMap, conv )
        return conv


    @classmethod
    def knownOptions( _class, only = (), hide = (), pairConv = False ):
        """return dictionary of options and default values known by this class.
        Optional arguments `only` and `hide` allow passing lists of regex strings for filtering options.
        If `pairConv` is set True, the options default value get paired with its converter.
        """
        opts = _class._optsMap()
        if only or hide:
            import re
            chkOnly  = [re.compile(fr'^{p}$').match for p in only]
            chkHide  = [re.compile(fr'^{p}$').match for p in hide]
            selected = lambda s: (not chkOnly or any(c(s) for c in chkOnly)) and not any(c(s) for c in chkHide)
            opts     = dict( i for i in opts.items() if selected( i[0] ) )
        if pairConv:
            conv = _class._convMap()
            opts = { k: (v, conv.get( k, type(v) )) for k, v in opts.items() }
        return opts


    @classmethod
    def extractOpts( _class, opts, prioOpts = {}, optsMap = '__opts__', convMap = '__conv__' ):
        """return iterator yielding all known options, with values extracted from given dictionaries opts and prioOpts.
        Use the class attribute set by optsMap to build up the dictionary of known options.
        The values for the yielded options get retrieved with by the precedence: prioOpts, opts, defaults
        """
        conv = _class._convMap( convMap )
        null = []

        for optId, valDefault in _class._optsMap( optsMap ).items():
            vA, vB = _class._pickOpt( prioOpts, optId, null ), _class._pickOpt( opts, optId, valDefault )
            optVal = _class.resolveEnv( (vA, vB)[vA is null] )

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
        keyword arguments: known options get stored with higher precedence and might override values from argDict.
        """
        from ._helper import TypeObject
        self._opts = TypeObject( self.extractOpts( argDict, kwArgs ) )
