
from ctypes  import Union, sizeof, addressof, Structure, c_int8
from six     import add_metaclass
from ..tools import core_loader

try:
    from functools import reduce
except ImportError:
    pass


######################################
class _Meta(type(Union)):
######################################

    def __new__( _class, name, bases, members ):
        from operator    import add
        #from collections import OrderedDict

        def _mro( ident, default ):
            scopes = (members,) + tuple( b.__dict__ for b in bases )
            return list(filter( None, (s.get( ident ) for s in scopes) )) + [default]

        typeName = _mro( '__typename__', name )[0]
        method   = '{0}_{{0}}c_'.format(typeName.lower())

        # collect list of __typeprocs__ from members and base classes ...
        members.setdefault( '__typeprocs__', [method] )
        procList = reduce( add, _mro( '__typeprocs__', [] ) )
        members['__typeprocs__'] = procList #list( OrderedDict.fromkeys( procList ) )

        # 'inherit' fields of base classes by reserving data space...
        fields = list( members.pop('_fields_', []) ) \
               + list( ('_data.' + b.__name__, b._data.size * c_int8) for b in bases if hasattr(b, '_data') )
        anonym = list( members.pop('_anonymous_', []) )
        size   = getattr( core_loader.handle, method.format('object_size_'), lambda: 0 )()

        if fields:
            _Struct = type( '_Struct', (Structure,), dict(_fields_=fields, _anonymous_=anonym) )
            size    = max(size, sizeof(_Struct))
            fields  = [('_struct', _Struct)]
            anonym  = ['_struct']

        # extend bases by abstract interface classes to allow isinstance-checks
        bases += tuple( members.get('__abstract__', []) )

        size and fields.append( ('_data', size * c_int8) )
        members.update( _fields_=fields, _anonymous_=anonym )
        return super(_Meta, _class).__new__( _class, name, bases, members )



@add_metaclass(_Meta)
######################################
class Compound(Union):
######################################
    __typeprocs__ = [] #< no procedures for Compound
    __abstract__  = [] #< no abstract bases
    __slots__     = ['_needs_delete', '_py_data']
    __py_cache__  = dict()

    @classmethod
    def __getattr__( _class, name ):
        if name == '_needs_delete': #< if we end up here, slot _needs_delete has not been set!
            return False
        if name in ('__members__', '__methods__'):
            return {}

        for fmt in _class.__typeprocs__:
            try   : attr = getattr(core_loader.handle, fmt.format(name)); break
            except: pass
        else:
            raise AttributeError("'%s' object has no attribute '%s'" % (_class.__name__, name))
        setattr(_class, name, attr)
        return attr


    @property
    def pyData( self ):
        try   : return self.__getattribute__('_py_data')
        except:
            self._py_data = Compound.__py_cache__.setdefault( addressof(self), dict() )
            return self._py_data


    def __new__( _class, *args, **kwArgs ):
        self = super(Compound, _class).__new__( _class, *args, **kwArgs )
        self._needs_delete = True
        return self


    def __del__( self ):
        if self._needs_delete:
            Compound.__py_cache__.pop( addressof(self), None )
            self.delete()


    def __hash__( self ):
        return hash( tuple(self._data) )


    def delete( self ): #< reimplemented in Object
        pass


from functools import wraps

######################################
def pyData_property( f ):
######################################
    @wraps( f )
    def _wrapper( self ):
        try  : return self.pyData['.' + f.__name__]
        except KeyError:
            self.pyData['.' + f.__name__] = val = f( self )
            return val
    return property( _wrapper )

