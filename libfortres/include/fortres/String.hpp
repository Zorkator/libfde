#ifndef __FORTRES_STRING_HPP
#define __FORTRES_STRING_HPP

#include <string>
#include <algorithm>
#include <functional>
#include <sstream>
#include <cctype>
#include "fortres/Array.hpp"

namespace fortres
{

  class String
  : public std::string
  {
    public:
      typedef Array<String>   List;

    public:
        String( const char *other = NULL )
        : std::string( (other == NULL)? "" : other )
          { /* nothing to do here */ }

        String( const std::string &other )
        : std::string( other )
          { /* nothing to do here */ }

        String( size_t n, char ch )
        : std::string( n, ch )
          { /* nothing to do here */ }

      template <typename TYPE>
        explicit
        String( const TYPE &val )
        {
          std::ostringstream stream;
          stream << val;
          (*this) = stream.str();
        }

      template <typename TYPE>
      TYPE
        as( void ) const
        {
          TYPE value = TYPE();
          std::stringstream valueStream( *this );
          valueStream >> value;
          return value;
        }

      bool
        startsWith( const std::string &start ) const
        {
          int idx = -1;
          if (this->length() >= start.length())
            { idx = this->std::string::compare( 0, start.length(), start ); }
          return (idx == 0);
        }

      bool
        endsWith( const std::string &ending ) const
        {
          int idx = -1;
          if (this->length() >= ending.length())
            { idx = this->std::string::compare( (this->length() - ending.length()), ending.length(), ending ); }
          return (idx == 0);
        }

      String &
        toUpperCase( void )
        {
          std::transform( this->begin(), this->end(), this->begin(), ::toupper );
          return (*this);
        }

      String &
        toLowerCase( void )
        {
          std::transform( this->begin(), this->end(), this->begin(), ::tolower );
          return (*this);
        }

      String &
        replaceBegin( const std::string &start, const std::string &replacement )
        {
          if (this->startsWith( start ))
            { this->std::string::replace( 0, start.length(), replacement ); }
          return (*this);
        }

      String &
        replaceEnd( const std::string &ending, const std::string &replacement )
        {
          if (this->endsWith( ending ))
            { this->std::string::replace( (this->length() - ending.length()), ending.length(), replacement ); }
          return (*this);
        }

      String &
        replace( const std::string &pattern, const std::string &replacement )
        {
          size_t pos = 0;
          do
          {
            if ((pos = this->find( pattern, pos )) == std::string::npos)
              { break; }
            this->std::string::replace( pos, pattern.length(), replacement );
            pos += replacement.length();
          } while (true);
          return (*this);
        }

      String &
        replaceAllOf( const char *charSet, char newChar )
        {
          size_t pos = 0;
          do
          {
            if ((pos = this->find_first_of( charSet, pos )) == std::string::npos)
              { break; }
            (*this)[pos++] = newChar;
          } while (true);
          return (*this);
        }

      String &
        trimBegin( void )
        {
          this->erase( this->begin(), std::find_if( this->begin(), this->end(), []( int character ) {
              return !std::isspace( character );
          } ) );
          return (*this);
        }

      String &
        trimBegin( const char *trimChars )
        {
          this->erase( 0, this->find_first_not_of( trimChars ) );
          return (*this);
        }

      String &
        trimEnd( void )
        {
          this->erase( std::find_if( this->rbegin(), this->rend(), []( int character ) {
              return !std::isspace( character );
          } ).base(), this->end() );
          return (*this);
        }

      String &
        trimEnd( const char *trimChars )
        {
          this->erase( this->find_last_not_of( trimChars ) + 1 );
          return (*this);
        }

      String &
        trim( void )
          { return (*this).trimBegin().trimEnd(); }

      String &
        trim( const char *trimChars )
          { return (*this).trimBegin( trimChars ).trimEnd( trimChars ); }

      String &
        cutBegin( size_t n )
        {
          this->erase( this->begin(), this->begin() + n );
          return (*this);
        }

      String &
        cutEnd( size_t n )
        {
          this->erase( this->end() - n, this->end() );
          return (*this);
        }

      String &
        cutBegin( const std::string &str )
        {
          if (this->startsWith( str ))
            { this->cutBegin( str.length() ); }
          return (*this);
        }

      String &
        cutEnd( const std::string &str )
        {
          if (this->endsWith( str ))
            { this->cutEnd( str.length() ); }
          return (*this);
        }

      String &
        cutAtFirstOf( const char *cutChars, bool keepCutter = true )
        {
          size_t cutPos = this->find_first_of( cutChars );
          if (cutPos != std::string::npos)
            { this->erase( 0, cutPos + !keepCutter ); }
          return (*this);
        }

      String &
        cutAtLastOf( const char *cutChars, bool keepCutter = true )
        {
          size_t cutPos = this->find_last_of( cutChars );
          if (cutPos != std::string::npos)
            { this->erase( cutPos + keepCutter ); }
          return (*this);
        }

      String &
        operator << ( const char *cstr )
        {
          this->std::string::append( (cstr != NULL)? cstr : "" );
          return (*this);
        }

      String &
        operator << ( const std::string &other )
        {
          this->std::string::append( other.c_str() );
          return (*this);
        }

      /**
       * const variants, modifying and returning a copy
       */
      String
        toUpperCase( void ) const
          { return String( *this ).toUpperCase(); }

      String
        toLowerCase( void ) const
          { return String( *this ).toLowerCase(); }

      String
        replaceBegin( const std::string &start, const std::string &replacement ) const
          { return String( *this ).replaceBegin( start, replacement ); }

      String
        replaceEnd( const std::string &ending, const std::string &replacement ) const
          { return String( *this ).replaceEnd( ending, replacement ); }

      String
        replace( const std::string &pattern, const std::string &replacement ) const
          { return String( *this ).replace( pattern, replacement ); }

      String
        replaceAllOf( const char *charSet, char newChar ) const
          { return String( *this ).replaceAllOf( charSet, newChar ); }

      String
        trimBegin( void ) const
          { return String( *this ).trimBegin(); }

      String
        trimBegin( const char *trimChars ) const
          { return String( *this ).trimBegin( trimChars ); }

      String
        trimEnd( void ) const
          { return String( *this ).trimEnd(); }

      String
        trimEnd( const char *trimChars ) const
          { return String( *this ).trimEnd( trimChars ); }

      String
        trim( void ) const
          { return String( *this ).trim(); }

      String
        trim( const char *trimChars ) const
          { return String( *this ).trim( trimChars ); }

      String
        cutBegin( size_t n ) const
          { return String( *this ).cutBegin( n ); }

      String
        cutEnd( size_t n ) const
          { return String( *this ).cutEnd( n ); }

      String
        cutBegin( const std::string &str ) const
          { return String( *this ).cutBegin( str ); }

      String
        cutEnd( const std::string &str ) const
          { return String( *this ).cutEnd( str ); }

      String
        cutAtFirstOf( const char *cutChars, bool keepCutter = true ) const
          { return String( *this ).cutAtFirstOf( cutChars, keepCutter ); }

      String
        cutAtLastOf( const char *cutChars, bool keepCutter = true ) const
          { return String( *this ).cutAtLastOf( cutChars, keepCutter ); }

      String
        operator << ( const char *cstr ) const
          { return String( *this ) << cstr; }

      String
        operator << ( const std::string &other ) const
          { return String( *this ) << other; }

      List
        split( const String &sep, bool keepEmptyParts = true, bool doAppend = false ) const
        {
          List list;
          this->split( sep, &list, keepEmptyParts, doAppend );
          return list;
        }

      size_t
        split( const String &sep, List *list, bool keepEmptyParts = true, bool doAppend = false ) const
        {
          size_t scanIdx = 0, idx, len;

          if (!doAppend)
            { list->clear(); }
          do
          {
            len = idx = this->find( sep, scanIdx );

            if (len != std::string::npos)
              { len -= scanIdx; }
            if (keepEmptyParts || (len != 0 && scanIdx < this->size()))
              { (*list) << this->substr( scanIdx, len ); }
            scanIdx = idx + sep.length();
          } while (idx != std::string::npos);
          return list->size();
        }
  };

} /* namespace fortres */

#endif /* __FORTRES_STRING_HPP */

