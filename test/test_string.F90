
#include "fde/exception.fpp"

program testinger
  use fde_string
  use fde_ref
  use fde_exception
  implicit none

  type(String_t)          :: tmp
  type(String_t)          :: ds, ds2, ds3
  type(String_t)          :: strings(4)
  type(String_t), pointer :: strPtr => null()
  type(Ref_t)             :: strRef, gref
  character               :: buffer(10), bufferB(10)
  character               :: buffer2(5)
  character(len=32)       :: stringArray(20)
  character(len=10)       :: strMat(10,20)

  integer :: i, idx, jdx

  call set_attribute( tmp, attribute_volatile )
  call set_attribute( tmp, attribute_permanent )

# define _testStr   'test string'
# define _shortStr  'short'
# define _spacedStr '   text   '
# define _longStr   'some long test string of more than ten characters ...'
# define _frame(x)  '#' // x // '#'

# define _cmp(this,to)              \
    _assert( str(this) == to )     ;\
    _assert( len(this) == len(to) )


  ds = String( _testStr )
  _cmp( ds, _testStr )
  _cmp( ds2, '' )             !< test null string

  ds2 = ds                    !< assignment to null string
  ds2 = _shortStr             !< assignment of shorter character string (no re-allocation!)
  _cmp( ds2, _shortStr )      !< shorter string from original buffer ...

  ds  = ds                    !< self assignment (no effect!)
  _cmp( ds, _testStr )

  ds2 = ds3                   !< assign null string (cleared but no deallocation!)
  _cmp( ds2, '' )             !< print cleared string and its length

  ds2 = ds // ds2             !< concat and reassign
  _cmp( ds2, _testStr )

  ! string concatenation ...
  _assert( (ds2 // ' concat') == (_testStr // ' concat') )
  _assert( ('prefix ' // ds2) == ('prefix ' // _testStr) )
  _assert( (ds2 // ds2)       == (_testStr // _testStr) )

  _assert( (ds3 .eq. String('foo'))              .eqv. .false. )
  _assert( ('bar' == ds3)                        .eqv. .false. )
  _assert( (ds2 // 'bla' .eq. String(char(ds3))) .eqv. .false. )


  ds2 = _longStr
  _assert( char(ds2) == _longStr )
  _assert( char(ds2, 10) == _longStr(:10) )

  ds2 = _testStr
  _assert( char(ds2) == _testStr )
  _assert( (char(ds2,  5) // '#') == _testStr(:5) // '#' )
  _assert( (char(ds2, 20) // '#') == _testStr // repeat(' ', 20 - len(ds2)) // '#' )

  ds2 = _spacedStr
  _assert( all(iachar(ds2) == [32,32,32,116,101,120,116,32,32,32]) )
  _assert( all(iachar(ds2) == ichar(ds2)) )
  _assert( _frame(adjustr(ds2)) == _frame(adjustr(_spacedStr)) )
  _assert( _frame(adjustl(ds2)) == _frame(adjustl(_spacedStr)) )
  _assert( _frame(adjustr(String('text'))) == _frame('text') )
  _assert( _frame(adjustl(String('      test '))) == _frame('test       ') )

  _assert( _frame(trim(ds2)) == _frame(trim(_spacedStr)) )
  _assert( _frame(strip(ds2)) == _frame('text') )

  ds2 = '     '
  _assert( _frame(ds2) == '#     #' )
  _assert( _frame(strip(ds2)) == _frame('') )

  ds2 = 'test/test_string.F90' !< can't use __FILE__, its' content is compiler dependent
  _assert( str(ds2) == 'test/test_string.F90' )
  _assert( file_dirname( ds2 ) == 'test/' )
  _assert( file_basename( ds2 ) == 'test_string' )

  ds2    = "abcdef"
  buffer = ds2
  _assert( all(buffer == transfer( 'abcdef    ', buffer )) )
  buffer = 'x'
  buffer(:4) = achar(ds2, 4)
  _assert( all(buffer == transfer( 'abcdxxxxxx', buffer )) )
  buffer(:len(ds2)) = achar(ds2)
  _assert( all(buffer == transfer( 'abcdefxxxx', buffer )) )
  buffer = achar(ds2, size(buffer))
  _assert( all(buffer == transfer( 'abcdef    ', buffer )) )

  _assert( str(String('testinger')) == '' ) !< str-conversion of temporary-String
  ds2 = String( buffer ) // ' appendix'
  _assert( ds2 == 'abcdef     appendix' )
  ds2 = buffer
  _assert( ds2 == 'abcdef    ' )

  !print *, cptr(ds2)
  strRef = ref_of(ds2)
  print *, is_String(strRef)
  ds = String(strRef)
  print *, char(String(strRef))

  strPtr => String(strRef)
  strPtr = '  x' // strPtr // 'x  '
  print *, str(ds2)

  gref = clone(strRef)
  !if (is_ref(gref)) then
    if (is_String(gref)) &
      print *, str(String(gref))
  !end if
  call free(gref)

#define T .true.
#define F .false.

  _assert( all( lge( buffer, achar(ds2, size(buffer)) ) .eqv. [T,T,F,T,T,T,F,F,F,T] ) )
  print *, lge( ds2, 'abcdef' )
  print *, lge( ds2, 'abxdxf' )

  do i = 1, 1000
    idx = mod( i,   size(strings) ) + 1
    jdx = mod( i+7, size(strings) ) + 1

    if (iand( i, 1 ) > 0) then
      strings(jdx) = strings(idx)
      strings(idx) = "another string"
    else
      strings(jdx) = String("long test string, converted to soft String")
      strings(idx) = "yet another test string"
    end if
  end do

  do i = 1, size(strings)
    call delete( strings(i) )
  end do

  call delete( ds )
  call delete( ds2 )
  call delete( ds3 )
  call delete( strRef )
  call delete( gref )

  call acceptStringArray( stringArray )
  call acceptStringMatrix( strMat )

  contains

  subroutine acceptStringArray( x )
    character(*), dimension(:) :: x
  end subroutine

  subroutine acceptStringMatrix( x )
    character(*), dimension(:,:) :: x
  end subroutine

end

