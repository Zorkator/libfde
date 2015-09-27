
module adt_ostream
  implicit none
  private

  type ostream_t
    procedure(), nopass, pointer :: drainFunc => null()
    integer                      :: channel   =  6
    character(len=16)            :: fmt       =  "(A     ,     /$)"
  end type

  interface ostream ; module procedure ostream_create  ; end interface
  interface width   ; module procedure ostream_width   ; end interface
  interface newline ; module procedure ostream_newline ; end interface
  interface write   ; module procedure ostream_write   ; end interface

  public :: ostream_t
  public :: ostream, width, newline, write

  contains

  pure &
  function ostream_create( chnl ) result(res)
    integer, intent(in) :: chnl
    type(ostream_t)     :: res
    res%drainFunc => ostream_write
    res%channel   = chnl
    res%fmt       = "(A     ,     /$)"
  end function


  subroutine ostream_width( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num

    if (num > 0) then; write(ostr%fmt(3:7), '(I5)') num
                 else;       ostr%fmt(3:7) = ' '
    endif
  end subroutine


  subroutine ostream_newline( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num

    if (num > 0) then; write(ostr%fmt(9:14), 100) num
                 else;       ostr%fmt(9:14) = ' '
    endif
100 format(I5,'/')
  end subroutine

  
  subroutine ostream_write( ostr, str )
    type(ostream_t)  :: ostr
    character(len=*) :: str
    write( ostr%channel, ostr%fmt ) str
  end subroutine

end module

