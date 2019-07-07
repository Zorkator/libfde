
#####################################
class Variable(object):
#####################################

    @property
    def value( self ):
        try   : return self._ref.value
        except: return self._ref

    @value.setter
    def value( self, val ):
        self._ref.value = val

    def __init__( self, ref ):
        self._ref = ref

    # rich comparison methods
    def __lt__( self, other )     : return self.value <  other
    def __le__( self, other )     : return self.value <= other
    def __eq__( self, other )     : return self.value == other
    def __ne__( self, other )     : return self.value != other
    def __gt__( self, other )     : return self.value >  other
    def __ge__( self, other )     : return self.value >= other

    # basic arithmetic methods
    def __add__( self, other )    : return self.value +  other
    def __sub__( self, other )    : return self.value -  other
    def __mul__( self, other )    : return self.value *  other
    def __div__( self, other )    : return self.value /  other
    def __pow__( self, other )    : return self.value ** other
    def __mod__( self, other )    : return self.value %  other

    # basic arithmetic methods (reversed)
    def __radd__( self, other )   : return other +  self.value
    def __rsub__( self, other )   : return other -  self.value
    def __rmul__( self, other )   : return other *  self.value
    def __rdiv__( self, other )   : return other /  self.value
    def __rpow__( self, other )   : return other ** self.value
    def __rmod__( self, other )   : return other %  self.value

    # basic arithmetic methods (inplace)
    def __iadd__( self, other )   : self.value  += other; return self
    def __isub__( self, other )   : self.value  -= other; return self
    def __imul__( self, other )   : self.value  *= other; return self
    def __idiv__( self, other )   : self.value  /= other; return self
    def __imod__( self, other )   : self.value  %= other; return self
    def __ipow__( self, other )   : self.value **= other; return self

    # divmod + reversed
    def __divmod__( self, other ) : return divmod( self.value, other )
    def __rdivmod__( self, other ): return divmod( other, self.value )

    # bit arithmetic methods
    def __and__( self, other )    : return self.value &  other
    def __xor__( self, other )    : return self.value ^  other
    def __or__( self, other )     : return self.value |  other
    def __lshift__( self, other ) : return self.value << other
    def __rshift__( self, other ) : return self.value >> other

    # bit arithmetic methods (reversed)
    def __rand__( self, other )   : return other & self.value
    def __rxor__( self, other )   : return other ^ self.value
    def __ror__( self, other )    : return other | self.value
    def __rlshift__( self, other ): return other << self.value
    def __rrshift__( self, other ): return other >> self.value

    # bit arithmetic methods (inplace)
    def __iand__( self, other )   : self.value |=  other; return self
    def __ixor__( self, other )   : self.value ^=  other; return self
    def __ior__( self, other )    : self.value |=  other; return self
    def __ilshift__( self, other ): self.value <<= other; return self
    def __irshift__( self, other ): self.value >>= other; return self

    # unary operators
    def __neg__( self )           : return -self.value
    def __pos__( self )           : return +self.value
    def __abs__( self )           : return abs(self.value)
    def __invert__( self )        : return ~self.value

    # conversion
    def __complex__( self )       : return complex(self.value)
    def __int__( self )           : return int(self.value)
    def __long__( self )          : return long(self.value)
    def __float__( self )         : return float(self.value)

    # string representations
    def __str__( self )           : return str(self.value)
    def __repr__( self )          : return repr(self._ref)
    def __oct__( self )           : return oct(self.value)
    def __hex__( self )           : return hex(self.value)

    # item access
    def __getitem__( self, item )      : return self.value[item]
    def __setitem__( self, item, val ) : self.value[item] = val
    def __delitem__( self, item )      : del self.value[item]

