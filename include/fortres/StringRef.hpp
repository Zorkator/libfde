#ifndef __FORTRES_STRINGREF__HPP
#define __FORTRES_STRINGREF__HPP

#include <string>
#include <string.h>

#if defined _MSC_VER && _MSC_VER < 1600
  /* workaround for VC2008 */
  typedef unsigned __int32 uint32_t;
#else
# include <stdint.h>
#endif

#pragma pack(push, 4)
class StringRef
{
  public:
      StringRef( void ): _ref(NULL), _len(0)    { /* empty */ }
      StringRef( const char *cstr, size_t len ) { referTo( cstr, len ); }
      StringRef( const char *cstr )             { referTo( cstr ); }
      StringRef( std::string &str )             { referTo( str ); }
      StringRef( const StringRef &str )         { referTo( str ); }

    const StringRef &
      assignTo( char *cstr, size_t len )
      {
        len = (len < _len)? len : _len;
        memcpy( cstr, _ref, len );
        memset( cstr + len, ' ', (_len - len) );
        return *this;
      }

    void
      assignTo( std::string &str ) const
        { str.assign( _ref, _len ); }


    StringRef
      concat( const char *cstr, size_t len )
      {
        StringRef res;
        if (_ref != NULL)
        {
          len = (_len < len)? _len : len;
          memcpy( this->buffer(), cstr, len );
          res.referTo( _ref + len, _len - len );
        }
        return res;
      }

    StringRef
      concat( const char *cstr )
        { return this->concat( cstr, strlen(cstr) ); }

    StringRef
      concat( const std::string &str )
        { return this->concat( str.c_str(), str.length() ); }

    StringRef
      concat( const StringRef &str )
        { return this->concat( str.buffer(), str.length() ); }


    StringRef &
      pad( size_t shift = 0 )
      {
        if (_ref != NULL)
        {
          shift = (shift < _len)? shift : _len;
          memset( this->buffer() + shift, ' ', (_len - shift) );
        }
        return *this;
      }


    StringRef &
      referTo( const char *ref, size_t len )
      {
        _ref = ref;
        _len = len;
        return *this;
      }

    StringRef &
      referTo( const char *cstr )
        { return this->referTo( cstr, strlen(cstr) ); }

    StringRef &
      referTo( const std::string &str )
        { return this->referTo( str.c_str(), str.length() ); }

    StringRef &
      referTo( const StringRef &str )
        { return this->referTo( str.buffer(), str.length() ); }


    std::string
      str( void ) const
        { return std::string( _ref, _len ); }

    std::string
      trim( void ) const
        { return this->trimmed().str(); }

    StringRef
      trimmed( void ) const
      {
        const char *beg = _ref, *end = _ref;
        if (beg != NULL)
        {
          for (end = beg + _len; beg < end && *beg == ' '; ++beg) { /* empty */ }
          for (end--           ; beg < end && *end == ' '; --end) { /* empty */ }
		  /* NOTE: here end-pointer is one too short! */
        }
        return StringRef( beg, (end - beg) + 1 /*<< +1 compensates end */ );
      }

    void
      erase( void )
      {
        _ref = NULL;
        _len = 0;
      }

    char *
      buffer( void ) const
        { return const_cast<char *>(_ref); }

    size_t
      length( void ) const
        { return _len; }

  private:
    const char *_ref;
    size_t      _len;
};
#pragma pack(pop)

#endif /* __FORTRES_STRINGREF__HPP */

