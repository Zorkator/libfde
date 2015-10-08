#ifndef __ADT_SCOPE_FPP
#define __ADT_SCOPE_FPP

#include "adt/ppUtil.xpp"
#include "adt/string.fpp"

# define _this_scopeId() \
    file_basename( __FILE__ )


# define _get_scope( parent, scopeId ) \
    getScope( parent, _strip(scopeId) )

# define _get_scope2( parent, a, b ) \
    getScope( parent, _strip(a), _strip(b) )

# define _get_scope3( parent, a, b, c ) \
    getScope( parent, _strip(a), _strip(b), _strip(c) )

# define _get_scope4( parent, a, b, c, d ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d) )

# define _get_scope5( parent, a, b, c, d, e ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e) )

# define _get_scope6( parent, a, b, c, d, e, f ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f) )

# define _get_scope7( parent, a, b, c, d, e, f, g ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g) )

# define _get_scope8( parent, a, b, c, d, e, f, g, h ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g), _strip(h) )

# define _get_scope9( parent, a, b, c, d, e, f, g, h, i ) \
    getScope( parent, _strip(a), _strip(b), _strip(c), _strip(d), _strip(e), _strip(f), _strip(g), _strip(h), _strip(i) )


# define _this_scope_in( parent ) \
    _get_scope( parent, _this_scopeId() )

# define _this_scope() \
    _this_scope_in( _rootScope_ )


# define _set_scopeSymbol_as( parent, sym, id ) \
    call set( parent, id, Item_of(ref_of(sym)) )

# define _set_scopeSymbol( parent, sym ) \
    _set_scopeSymbol_as( parent, sym, _str(sym) )

# define _set_scopeValue_as( parent, sym, id ) \
    call set( parent, id, Item_of(sym) )

# define _set_scopeValue( parent, sym ) \
    _set_scopeValue_as( parent, sym, _str(sym) )


# define _remove_scopeSymbol( parent, id ) \
    call remove( parent, id )

#endif

