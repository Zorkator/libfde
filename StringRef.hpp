#ifndef __FORTRES_STRINGREF__HPP
#define __FORTRES_STRINGREF__HPP

#include <string>
#include <string.h>

#if defined _MSC_VER
  typedef unsigned __int32  uint32_t;
#else
# include <stdint.h>
#endif

#pragma pack(push, 4)
class StringRef
{
  public:
    const StringRef &
      operator = ( const std::string &str )
      {
        if (_ref != NULL)
        {
          size_t l = str.length();
          if (l < _len)
          {
            memset( _ref + l, ' ', _len - l );
            _len = l;
          }
          memcpy( _ref, str.c_str(), _len );
        }
        return *this;
      }

    void
      assignTo( std::string &str ) const
        { str.assign( _ref, _len ); }

    void
      erase( void )
      {
        _ref = NULL;
        _len = 0;
      }

  private:
    char     *_ref;
    uint32_t  _len;
};
#pragma pack(pop)

#endif /* __FORTRES_STRINGREF__HPP */

