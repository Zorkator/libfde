#ifndef __FORTRES_STRINGREF__HPP
#define __FORTRES_STRINGREF__HPP

#include <string>
#include <string.h>

#if defined _MSC_VER && _MSC_VER >= 1600
#	include <stdint.h>
#else
	typedef unsigned __int32 uint32_t;
#endif

#pragma pack(push, 4)
class StringRef
{
  public:
      StringRef( void ): _ref(NULL), _len(0) { /* empty */ }
      StringRef( const char *cstr )          { referTo( cstr ); }
      StringRef( std::string &str )          { referTo( str ); }

    const StringRef &
      operator = ( const std::string &str )
      {
        if (_ref != NULL)
        {
          char  *ref = this->buffer();
          size_t l   = str.length();
          if (l < _len)
          {
            memset( ref + l, ' ', _len - l );
            _len = l;
          }
          memcpy( ref, str.c_str(), _len );
        }
        return *this;
      }

    void
      assignTo( std::string &str ) const
        { str.assign( _ref, _len ); }

    StringRef &
      referTo( const std::string &str )
      {
        _ref = str.c_str();
        _len = str.length();
        return *this;
      }

    StringRef &
      referTo( const char *ref )
      {
        _ref = ref;
        _len = strlen( ref );
        return *this;
      }

    std::string
      str( void ) const
        { return std::string( _ref, _len ); }

    std::string
      trim( void ) const
      {
        std::string res( _ref, _len );
        size_t      beg = res.find_first_not_of(" ");
        size_t      end = res.find_last_not_of(" ");
        beg = (beg != std::string::npos)? beg : 0;
        end = (end != std::string::npos)? end : _len;
        return res.substr( beg, end+1 );
      }

    void
      erase( void )
      {
        _ref = NULL;
        _len = 0;
      }

    char *
      buffer( void )
        { return const_cast<char *>(_ref); }

    size_t
      length( void )
        { return _len; }

  private:
    const char *_ref;
    size_t      _len;
};
#pragma pack(pop)

#endif /* __FORTRES_STRINGREF__HPP */

