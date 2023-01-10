
from ..tools import _decorate, _arg

#----------------------------
class Variable(object):
#----------------------------
    @property
    def value( self ):
        return self._ref

    @value.setter
    def value( self, val ):
        self._ref = val

    def _getValue( self, host ):
        return self.value

    def _setValue( self, host, value ):
        self.value = value

    def mkProperty( self ):
        "return getter/setter-property bound to this Variable"
        return property( self._getValue, self._setValue )

    @staticmethod
    def mkSetter( val, cmp=_arg ):
        """return function that sets a given variable to `val` and returns True.
        If `cmp` is given, the variable is set to `val` only if it's value equals to cmp.
        """
        if cmp is _arg:
            def _setter( var ):
                var.value = val
                return True
        else:
            def _setter( var ):
                if var.value == cmp:
                    var.value = val
                    return True
        return _setter


    def __new__( _class, ref, *args, **kwArgs ):
        try:
            ref.value
            try:
                len(ref)
                try   : ref[:]; _class = ValueVariable
                except:         _class = MappingVariable
            except:             _class = ValueVariable
        except:
            try:
                len(ref)
                try   : ref[:]; _class = ArrayVariable
                except:         _class = MappingVariable
            except:             _class = SimpleVariable
        #
        self = super(Variable, _class).__new__( _class )
        self._ref = ref
        self.__dict__.update( _decorate( kwArgs ) )
        return self


    def __getattr__( self, ident ):
        return getattr( self._ref, ident )

    # string representation
    def __repr__( self ):
        return "{}({})".format( type(self).__name__, repr(self._ref) )



try:
  _long = long
except NameError:
  _long = int

#---------------------------------
class SimpleVariable(Variable):
#---------------------------------
    # rich comparison methods
    def __lt__( self, other ):
        return self.value <  other
    def __le__( self, other ):
        return self.value <= other
    def __eq__( self, other ):
        return self.value == other
    def __ne__( self, other ):
        return self.value != other
    def __gt__( self, other ):
        return self.value >  other
    def __ge__( self, other ):
        return self.value >= other

    # basic arithmetic methods
    def __add__( self, other ):
        return self.value +  other
    def __sub__( self, other ):
        return self.value -  other
    def __mul__( self, other ):
        return self.value *  other
    def __truediv__( self, other ):
        return self.value /  other
    def __floordiv__( self, other ):
        return self.value // other
    def __pow__( self, other ):
        return self.value ** other
    def __mod__( self, other ):
        return self.value %  other
    __div__ = __truediv__  # < py2 compatibility

    # basic arithmetic methods (reversed)
    def __radd__( self, other ):
        return other +  self.value
    def __rsub__( self, other ):
        return other -  self.value
    def __rmul__( self, other ):
        return other *  self.value
    def __rtruediv__( self, other ):
        return other /  self.value
    def __rfloordiv__( self, other ):
        return other // self.value
    def __rpow__( self, other ):
        return other ** self.value
    def __rmod__( self, other ):
        return other %  self.value
    __rdiv__ = __rtruediv__  # < py2 compatibility

    # basic arithmetic methods (inplace)
    def __iadd__( self, other ):
        self.value  += other; return self
    def __isub__( self, other ):
        self.value  -= other; return self
    def __imul__( self, other ):
        self.value  *= other; return self
    def __itruediv__( self, other ):
        self.value  /= other; return self
    def __ifloordiv__( self, other ):
        self.value //= other; return self
    def __imod__( self, other ):
        self.value  %= other; return self
    def __ipow__( self, other ):
        self.value **= other; return self
    __idiv__ = __itruediv__  # < py2 compatibility

    # divmod + reversed
    def __divmod__( self, other ):
        return divmod( self.value, other )
    def __rdivmod__( self, other ):
        return divmod( other, self.value )

    # bit arithmetic methods
    def __and__( self, other ):
        return self.value &  other
    def __xor__( self, other ):
        return self.value ^  other
    def __or__( self, other ):
        return self.value |  other
    def __lshift__( self, other ):
        return self.value << other
    def __rshift__( self, other ):
        return self.value >> other

    # bit arithmetic methods (reversed)
    def __rand__( self, other ):
        return other & self.value
    def __rxor__( self, other ):
        return other ^ self.value
    def __ror__( self, other ):
        return other | self.value
    def __rlshift__( self, other ):
        return other << self.value
    def __rrshift__( self, other ):
        return other >> self.value

    # bit arithmetic methods (inplace)
    def __iand__( self, other ):
        self.value |=  other; return self
    def __ixor__( self, other ):
        self.value ^=  other; return self
    def __ior__( self, other ):
        self.value |=  other; return self
    def __ilshift__( self, other ):
        self.value <<= other; return self
    def __irshift__( self, other ):
        self.value >>= other; return self

    # unary operators
    def __neg__( self ):
        return -self.value
    def __pos__( self ):
        return +self.value
    def __abs__( self ):
        return abs(self.value)
    def __invert__( self ):
        return ~self.value

    # conversion
    def __complex__( self ):
        return complex(self.value)
    def __int__( self ):
        return int(self.value)
    def __long__( self ):
        return _long(self.value)
    def __float__( self ):
        return float(self.value)
    def __bool__( self ):
        return bool(self.value)
    __nonzero__ = __bool__        #< py2 compatibility

    # string representations
    def __oct__( self ):
        return oct(self.value)
    def __hex__( self ):
        return hex(self.value)



#---------------------------------
class ValueVariable(SimpleVariable):
#---------------------------------
    @property
    def value( self ):
        return self._ref.value

    @value.setter
    def value( self, val ):
        self._ref.value = val



#--------------------------------------
class _Iterable(Variable):
#--------------------------------------
    def __len__( self ):
        return len(self._ref)

    # item access
    def __getitem__( self, item ):
        return self._ref[item]
    def __setitem__( self, item, val ):
        self._ref[item] = val


#--------------------------------------
class ArrayVariable(_Iterable):
#--------------------------------------
    @property
    def value( self ):
        return self._ref[:]

    @value.setter
    def value( self, val ):
        self._ref[:] = val



#--------------------------------------
class MappingVariable(_Iterable):
#--------------------------------------
    # item access
    def __delitem__( self, item ):
        del self._ref[item]

