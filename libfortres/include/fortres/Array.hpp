
#ifndef __FORTRES_ARRAY_HPP
#define __FORTRES_ARRAY_HPP

#include <vector>
#include <algorithm>

namespace fortres
{

  /**
   * Eye candy template wrapper for stl vector.
   * It's sole perpose is to use stl classes while keeping the code readable.
   */
  template <typename ITEM_TYPE>
  class Array
  : public std::vector<ITEM_TYPE>
  {
    public:
      typedef ITEM_TYPE           Item; //< readable alternative to std::vector<ITEM_TYPE>::value_type
      typedef std::vector<Item>   SuperType;

    public:
        Array( void )
        : SuperType()
          { /* nothing to do here */ }

        Array( size_t n, const Item &init = Item() )
        : SuperType( n, init )
          { /* nothing to do here */ }

      bool
        addUnique( const Item &item )
        {
          bool wasAdded = false;

          typename SuperType::iterator pos = std::find( this->begin(), this->end(), item );
          if (pos == this->end())
          {
            this->push_back( item );
            wasAdded = true;
          }
          return wasAdded;
        }

      Array &
        remove( size_t idx )
        {
          this->erase( this->begin() + idx );
          return (*this);
        }

      bool
        removeItem( const Item &item )
        {
          bool wasRemoved = false;

          typename SuperType::iterator pos = std::find( this->begin(), this->end(), item );
          if (pos != this->end())
          {
            this->erase( pos );
            wasRemoved = true;
          }
          return wasRemoved;
        }

      size_t
        removeAny( const Item &item )
        {
          typename SuperType::iterator posEnd = std::remove( this->begin(), this->end(), item );
          size_t numRemoved = (this->end() - posEnd);

          this->erase( posEnd, this->end() );
          return numRemoved;
        }

      template <class Predicate>
      size_t
        removeIf( Predicate predicate )
        {
          typename SuperType::iterator posEnd = std::remove_if( this->begin(), this->end(), predicate );
          size_t numRemoved = (this->end() - posEnd);

          this->erase( posEnd, this->end() );
          return numRemoved;
        }

      bool
        hasItem( const Item &item ) const
          { return (std::find( this->begin(), this->end(), item ) != this->end()); }

      bool
        hasItems( const SuperType &items ) const
        {
          size_t idx = 0;
          while (idx < items.size() && this->hasItem( items[idx] ))
            { idx++; }
          return (idx == items.size());
        }

      bool
        contains( const Item &item ) const
          { return this->hasItem( item ); }

      bool
        contains( const SuperType &items ) const
          { return this->hasItems( items ); }

      Array &
        sort( void )
        {
          std::sort( this->begin(), this->end() );
          return (*this);
        }

      Array &
        removeDuplicates( void )
        {
          typename SuperType::iterator dupesBegin = std::unique( this->begin(), this->end() );
          this->erase( dupesBegin, this->end() );
          return (*this);
        }

      Array &
        operator << ( const Item &item )
        {
          this->push_back( item );
          return (*this);
        }

      Array &
        operator << ( const SuperType &other )
        {
          for (size_t idx = 0; idx < other.size(); ++idx)
            { this->push_back( other[idx] ); }
          return (*this);
        }

      Item
        value( size_t idx, const Item &defaultItem = Item() ) const
          { return ((idx < this->size())? (*this)[idx] : defaultItem ); }
  };

} /* namespace fortres */

#endif /* __FORTRES_ARRAY_HPP */

