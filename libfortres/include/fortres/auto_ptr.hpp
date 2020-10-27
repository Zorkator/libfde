#ifndef __FORTRES_AUTO_PTR_HPP
#define __FORTRES_AUTO_PTR_HPP

/**
 * A quick substitute for deprecated stl-version, broken in g++.
 */

namespace fortres
{

  template<typename T>
  class auto_ptr
  {
    public:
        typedef auto_ptr<T>   Type;

        auto_ptr( T *ptr = NULL )    : _ptr(ptr)        { /* empty */ }
        auto_ptr( const Type &other ): _ptr(other._ptr) { /* empty */ }

       ~auto_ptr( void )
          { if (_ptr) delete(_ptr); }

      T *
        release( void )
        {
          T *res = _ptr;
          _ptr = NULL;
          return res;
        }

      T *
        get( void )
          { return _ptr; }

      Type &
        reset( T *ptr )
        {
          if (_ptr != ptr)
          {
            if (_ptr) delete(_ptr);
            _ptr = ptr;
          }
          return *this;
        }

      Type &
        operator = ( T *ptr )
          { return this->reset( ptr ); }

    private:
      T *_ptr;
  };

} /* namespace fortres */

#endif /* __FORTRES_AUTO_PTR_HPP */

