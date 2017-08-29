
from os import path

####################################
class OptionProcessor(object):
####################################

  __conv__ = dict()
  __opts__ = dict( debug     = 0
                 , verbosity = 1
                 )

  @staticmethod
  def realpath( p ):
    return path.realpath( path.expanduser( p ) )

  @classmethod
  def _merge_class_attrib( _class, attrId ):
    """merge option dictionaries of class hierarchy."""
    attr = dict()
    for _c in reversed(_class.mro()):
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
      # pick value by priority: 1) dashed, 2) explicit, 3) default
      optVal = opts.pop(        optId, None ) or valDefault
      optVal = opts.pop( '--' + optId, None ) or optVal

      if isinstance( optVal, Exception ): raise optVal
      else                              : yield optId, conv.get( optId, type(valDefault) )( optVal )


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
    def _decorate( opt ): return '_' + opt[0], opt[1]

    # extract known options from kwArgs, and make them attributes ...
    self.__dict__.update( map( _decorate, self.extractOpts( kwArgs ) ) )

