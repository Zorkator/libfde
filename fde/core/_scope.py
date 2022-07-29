
from ._hashmap import HashMap
from ._ref     import Ref
from ._ftypes  import mappedType, _mapType, CALLBACK, CALLBACK_t, CFUNCTION_t, POINTER_t, VOID_Ptr
from ..tools   import dict2obj, _arg, auto_raise
from ctypes    import byref, c_char_p, c_int, POINTER, sizeof, Array
from six       import iteritems

try:
    from functools import reduce
except ImportError:
    pass


@mappedType( 'hashmap', 'type(HashMap_t)' )
#-------------------------------------------
class Scope( HashMap ):
#-------------------------------------------

    #-------------------------------------------
    class Index( HashMap.Index ):
    #-------------------------------------------
        def get( self ):
            key, valRef = super(type(self), self).get()
            return key, valRef.resolved


    def _mk_CFUNCTION( self, func, funcRetType = None, funcArgTypes = () ):
        ITF  = CFUNCTION_t( funcRetType, *funcArgTypes )
        func = func or 0
        if type(type(func)) is not type(ITF):
            func = ITF(func)
        return func, ITF

    def declareCallback( self, ident, argType = None ):
        ident = ident.encode()
        self.declare_callback_( byref(self), c_char_p(ident), c_int(len(ident)) )
        self[ident].pyData['itf'] = CALLBACK_t( argType )

    def typifyCallback( self, ident, argType ):
        ident = ident.encode()
        self[ident].pyData['itf'] = CALLBACK_t( argType )

    def _mk_CALLBACK( self, ident, func, remove = False ):
        cb  = self.getItem( ident, KeyError ).pyData
        ITF = cb.setdefault( 'itf', CALLBACK_t() )
        if not func:
            cfunc = ITF(0)
        elif type(type(func)) is not type(ITF):
            cfunc = cb.setdefault( id(func), ITF(func) )
            remove and cb.pop( id(func), None )
        return ident.encode(), cfunc

    def connectedCallbacks( self, ident ):
        return self.connected_callbacks_( byref(self), c_char_p(ident), c_int(len(ident)) )

    def connectCallback( self, ident, func ):
        ident, func = self._mk_CALLBACK( ident, func )
        return self.connect_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )

    #
    # compatibility
    setCallback = connectCallback

    def disconnectCallback( self, ident, func = None ):
        ident, func = self._mk_CALLBACK( ident, func, remove = True )
        return self.disconnect_callback_( byref(self), c_char_p(ident), func, c_int(len(ident)) )


    def tryCallback( self, ident, arg = None ):
        ident  = ident.encode()
        argPtr = VOID_Ptr() if arg is None else byref(arg)
        return self.try_callback_( byref(self), c_char_p(ident), argPtr, c_int(len(ident)) )


    def invokeCallback( self, ident, arg = None ):
        if self.tryCallback( ident, arg ) < 0:
            raise KeyError( "undeclared or disabled callback %s" % ident )


    def setProcedure( self, ident, func, retType = None, args = () ):
        cfunc, ITF = self._mk_CFUNCTION( func, retType, args )
        self.getItem( ident, KeyError ).pyData.update( itf = ITF )
        ident = ident.encode()
        self.set_procedure_( byref(self), c_char_p(ident), cfunc, c_int(len(ident)) )


    def getProcedure( self, ident, retType = _arg(None), args = _arg([]) ):
        ITF = self.getItem( ident, KeyError ).pyData.get( 'itf' )
        if not ITF or _arg.isGiven( retType ) or _arg.isGiven( args ):
            ITF = CFUNCTION_t( _arg.get( retType ), *_arg.get( args ) )
        proc  = ITF(0)
        ident = ident.encode()
        self.get_procedure_( byref(proc), byref(self), c_char_p(ident), VOID_Ptr(), c_int(len(ident)) )
        proc._itf = ITF
        return proc


    def _assign_tree( self, other, keyOp = None ):
        keyOp = keyOp or (lambda k: k)

        def _tree_walk( itemItr, stack ):
            for key, v in itemItr:
                if not isinstance( v, CALLBACK ):
                    k = keyOp( key )
                    try                  : _tree_walk( iteritems(v), stack + [stack[-1][k]] )
                    except AttributeError: stack[-1][k] = v

        _tree_walk( iteritems(other), [self] )


    def update( self, other = {}, **kwArgs ):
        self._assign_tree( dict(other) )
        self._assign_tree( kwArgs )

    def updateDomain( self, path_dict, keyOp=str.split ):
        self._assign_tree( path_dict, keyOp )


    def iterDomain( self, paths, keyOp = str.split, default = LookupError ):
        keyOp = keyOp or (lambda k: [k])
        for p in map( keyOp, paths ):
            try:
                yield p, self[p] if p else None
            except (TypeError, KeyError, AttributeError) as e:
                yield p, auto_raise( default, 'invalid scope path ' + str(p) )


    def extractDomain( self, paths, keyOp = str.split, default = LookupError ):
        def _put( d, k ): return d.setdefault( k, dict() )

        domain = dict()
        for p, v in self.iterDomain( paths, keyOp, default ):
            if p: reduce( _put, p[:-1], domain )[p[-1]] = v
        return domain

    def extractContext( self, paths, keyOp = str.split, default = LookupError ):
        return dict2obj( self.extractDomain( paths, keyOp, default ) )

    def asContext( self ):
        return dict2obj( self )


    def __getitem__( self, ident ):
        if isinstance( ident, (tuple, list) ):
            try:
                base = reduce( lambda d, k: d[k], ident[:-1], self )
                if type(type(base)) is type(Array): #< try on Scopes is expensive, so check for ctypes-Array!
                    return base._type_.from_buffer( base, int(ident[-1]) * sizeof( base._type_ ) )
                else:
                    return base[ident[-1]]
            except (TypeError, KeyError, AttributeError) as e:
                raise KeyError( 'invalid scope path ' + str(ident) )
        else:
            return super(Scope, self).__getitem__( ident )


    def __setitem__( self, ident, val ):
        if isinstance( ident, (tuple, list) ):
            base = reduce( lambda d, k: d[k], ident[:-1], self )
            if type(type(base)) is type(Array): #< try on Scopes is expensive, so check for ctypes-Array!
                base[int(ident[-1])] = val
            else:
                base[ident[-1]] = val
        else:
            return super(Scope, self).__setitem__( ident, val )


    def getSubScope( self, *path ):
        ptr = POINTER(type(self))(self)
        for ident in path:
            self.get_subscope_( byref(ptr), ptr, c_char_p(ident), c_int(len(ident)) )
        return ptr.contents


    @classmethod
    def getProcessScope( _class, *path ):
        ptr = POINTER(_class)()
        _class.__getattr__( 'get_processscope_' )( byref(ptr) )
        # resolve scope nesting of given path ...
        return reduce( _class.__getitem__, path, ptr.contents )


ScopePtr = POINTER_t( Scope )
_mapType( 'HashMapPtr', 'type(HashMapPtr_t)', ScopePtr )
