

####################################
class OptionProcessor(object):
####################################

  __opts__ = dict()

  @classmethod
  def knownOptions( _class, optionMapAttrib = '__opts__' ):
    """return dictionary of options and default values known by this class.
    Use the class attribute set by optionMapAttrib to build up the option dictionary.
    
    """
    try   : return _class._knownOpts
    except:
      # collect known class options ...
      opts = _class._knownOpts = dict()
      for _c in reversed(_class.mro()):
        opts.update( getattr( _c, optionMapAttrib, {} ) )
      return opts


  @classmethod
  def extractOpts( _class, opts, optionMapAttrib = '__opts__' ):
    """extract known options from given dictionary opts.
    Use the class attribute set by optionMapAttrib to build up the option dictionary.

    """
    for optId, valDefault in _class.knownOptions( optionMapAttrib ).items():
      # pick value by priority: 1) dashed, 2) explicit, 3) default
      optVal = opts.pop(        optId, valDefault )
      optVal = opts.pop( '--' + optId,  optVal )

      if isinstance( optVal, Exception ): raise optVal
      else                              : yield optId, optVal


  def __init__( self, **kwArgs ):
    """prepares instance by accepting and converting given arguments to attributes.
    
    keyword arguments:
     * All accepted 'dashed' arguments, as provided by the commadline, get renamed and stored as attributes.
       The attribute name is made by replacing the double dashes by one underscore , e.g.
         --libEnv = 'SOME_ENV_VAR' gets accessable by self._libEnv
     * Any 'dashed' arguments not recognized just get stored as attribute
         --unknown = 42 gets stored as self.__dict__['--unknown']
     * Any other arguments provided get decorated by underscore '_' and stored as attributes.
       Note that on name collision 'dashed' arguments are treated with priority!

    """
    def _decorate( s ): return s if s.startswith('--') else '_' + s

    # extract accepted 'dashed' arguments ...
    kwArgs.update( self.extractOpts( kwArgs ) )
    # rename remaining keyArgs and store them as attributes ...
    self.__dict__.update( zip( map( _decorate, kwArgs.keys() ), kwArgs.values() ) )

