
from ._helper  import TypeObject


#--------------------------------------------
class Caching( object ):
#--------------------------------------------
    """Mixin class that allows using cached_property.
    Caching stores the values of such properties in Wallet that gets excluded when pickling!
    """
    _preset = {'__stale__': {} }

    def __init__( self, *args, **kwArgs ):
        super( Caching, self ).__init__( *args, **kwArgs )
        self._stock = TypeObject( self._preset )

    def __getstate__( self ):
        state = self.__dict__.copy()
        state['_stock'] = TypeObject( self._preset )
        return state

    def cleanup( self ):
        for f in filter( callable, self._stock.__stale__.values() ):
            try   : f()
            except: pass
        self._stock.__stale__.clear()


def f_argcount( f ):
    """return number of arguments of given function `f` or None if `f` is not callable."""
    if callable(f):
        f = getattr( getattr( f, '__call__', f ), '__func__', f )  # < functors & staticmethods
        try:
            return f.__code__.co_argcount
        except AttributeError:
            f.__code__ = f.func_code
            return f.__code__.co_argcount


#------------------------------------------
class cached_property( property ):
#------------------------------------------
    """property decorator that caches the result in the object.
    """

    def __init__( self, *args, **kwArgs ):
        super(cached_property, self).__init__( *args, **kwArgs )
        self._xget = (f_argcount( self.fget ) or 0) == 2
        self._xset = (f_argcount( self.fset ) or 0) == 3
        self._xdel = (f_argcount( self.fdel ) or 0) == 2


    def __get__( self, obj, objType=None ):
        fg = self.fget
        if fg is None:
            raise AttributeError( "unreadable attribute" )

        if obj is None:
            return self
        try   : return obj._stock[fg.__name__]
        except:
            if self._xget: val = fg( obj, obj._stock.__stale__.pop( fg.__name__, None ) )
            else         : val = fg( obj )
            obj._stock[fg.__name__] = val
            return val


    def __set__( self, obj, value ):
        fg, fs = self.fget, self.fset
        if None in (fg, fs):
            raise AttributeError( "can't set attribute" )

        if self._xset: val = fs( obj, value, getattr( obj._stock, fg.__name__, None ) )
        else         : val = fs( obj, value )
        obj._stock[fg.__name__] = (val, value)[val is None]


    def __delete__( self, obj ):
        fg, fd = self.fget, self.fdel
        if None in (fg, fd):
            raise AttributeError( "can't delete attribute" )

        oldVal = vars(obj._stock).pop( fg.__name__, None )
        if self._xdel: val = fd( obj, oldVal )
        else         : val = fd( obj )
        if val:
            obj._stock.__stale__[fg.__name__] = val
