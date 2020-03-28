
program testinger
  use fde_string
  use fde_ref
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

  ds = String("test string")
  print *, str(ds), len(ds)   !< print string and its length
  print *, str(ds2), len(ds2) !< print null string and its length
  ds2 = ds                    !< assignment to null string
  ds2 = "short"               !< assignment of shorter character string (no re-allocation!)
  print *, str(ds2), len(ds2) !< print shorter string from original buffer ...
  ds  = ds                    !< self assignment (no effect!)
  ds2 = ds3                   !< assign null string (cleared but no deallocation!)
  print *, str(ds2), len(ds2) !< print cleared string and its length
  ds2 = ds // ds2             !< concat and reassign

  print *, str(ds2)
  print *, char(ds2)

  print *, ds2 // " concat"
  print *, "prefix " // ds2
  print *, ds2 // ds2

  print *, ds3 .eq. String("foo")
  print *, "bar" == ds3
  print *, ds2 // 'bla' .eq. String(char(ds3))

  ds2 = "some long test string of more than ten characters ..."
  print *, char(ds2)
  print *, char(ds2, 10)

  ds2 = "    short    "
  print *, char(ds2)
  print *, char(ds2, 5) // '#'
  print *, char(ds2, 20) // '#'
  print *, iachar(ds2)
  print *, ichar(ds2)
  print *, "#" // adjustr(ds2) // '#'
  print *, "#" // adjustl(ds2) // '#'
  print *, "#" // adjustr(String('text')) // '#'
  print *, "#" // adjustl(String('       text ')) // '#' // String('usw')

  print *, trim(ds2)//'<<'
  print *, file_dirname( __FILE__ )
  print *, file_basename( __FILE__ )
  ds2 = __FILE__
  print *, file_dirname( ds2 )
  print *, file_basename( ds2 )
  print *, file_dirname( string(__FILE__) )
  print *, file_basename( string(__FILE__) )
  print *, str( string(__FILE__) )
  ds2 = ds2 // "                                       "
  print *, trim(ds2)//'<<'
  ds2 = ' testinger  '
  print *, trim(ds2)//'<<'
  ds2 = ''
  print *, trim(ds2)//'<<'
  print *, trim( string(' testinger     ') )//'<<'
  ds2 = ''
  print *, '>>'//strip(ds2)//'<<'
  print *, strip( string('    testinger     ') )//'<<'
  ds2 = ' testinger  '
  print *, '>>'//strip(ds2)//'<<'

  ds2 = "abcdef"

  buffer = ' '
  buffer(:len(ds2)) = achar(ds2)
  buffer(:4) = achar(ds2, 4)
  buffer = achar(ds2, 10)
  print *, buffer

  print *, str(String('testinger'))
  ds2 = String( buffer ) // " appendix"
  ds2 = buffer

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

  print *, lge( buffer, achar(ds2, size(buffer)) )
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

