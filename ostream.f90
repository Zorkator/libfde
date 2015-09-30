
#include "adt/itfUtil.fpp"

module adt_ostream
  implicit none
  private

  type, public :: ostream_t
    procedure(), nopass, pointer :: drainFunc => null()
    integer                      :: channel   =  6
    character(len=16)            :: fmt_      =  "(A     ,     /$)"
    integer                      :: indents_  =  0
    
  end type

  interface ostream ; module procedure ostream_create  ; end interface
  interface width   ; module procedure ostream_width   ; end interface
  interface newline ; module procedure ostream_newline ; end interface
  interface indent  ; module procedure ostream_indent  ; end interface
  interface write   ; module procedure ostream_write   ; end interface

  public :: ostream, width, newline, indent, write

  contains

!_PROC_EXPORT(ostream_create)
  pure &
  function ostream_create( chnl ) result(res)
    integer, intent(in) :: chnl
    type(ostream_t)     :: res
    res%drainFunc => ostream_write
    res%channel   = chnl
  end function


!_PROC_EXPORT(ostream_width)
  subroutine ostream_width( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num

    if (num > 0) then; write(ostr%fmt_(3:7), '(I5)') num
                 else;       ostr%fmt_(3:7) = ' '
    endif
  end subroutine


!_PROC_EXPORT(ostream_indent)
  subroutine ostream_indent( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num
    ostr%indents_ = num 
  end subroutine

  
!_PROC_EXPORT(ostream_newline)
  subroutine ostream_newline( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num

    if (num > 0) then; write(ostr%fmt_(9:14), 100) num
                 else;       ostr%fmt_(9:14) = ' '
    endif
100 format(I5,'/')
  end subroutine

  
!_PROC_EXPORT(ostream_write)
  subroutine ostream_write( ostr, str )
    type(ostream_t)  :: ostr
    character(len=*) :: str

    if (ostr%indents_ > 0) &
      write( ostr%channel, '(A$)' ) repeat("  ", ostr%indents_)
    write( ostr%channel, ostr%fmt_ ) str
  end subroutine

end module

