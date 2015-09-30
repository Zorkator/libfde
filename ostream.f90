
#include "adt/itfUtil.fpp"

module adt_ostream
  implicit none
  private

  type, public :: ostream_t
    procedure(), nopass, pointer :: drainFunc => null()
    integer                      :: channel   =  6
    character(len=10)            :: fmt_      =  "(A     $)"
    character(len=10)            :: nl_       =  "(     /$)"
    integer                      :: indents_  =  0
    integer                      :: fuseCnt_  =  0
  end type

  interface ostream   ; module procedure ostream_create    ; end interface
  interface width     ; module procedure ostream_width     ; end interface
  interface newline   ; module procedure ostream_newline   ; end interface
  interface indent    ; module procedure ostream_indent    ; end interface
  interface fuselines ; module procedure ostream_fuselines ; end interface
  interface write     ; module procedure ostream_write     ; end interface

  public :: ostream, width, newline, indent, fuselines, write

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

  
!_PROC_EXPORT(ostream_fuselines)
  subroutine ostream_fuselines( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num
    ostr%fuseCnt_ = max( 0, ostr%fuseCnt_ + num - 1  )
  end subroutine

  
!_PROC_EXPORT(ostream_newline)
  subroutine ostream_newline( ostr, num )
    type(ostream_t) :: ostr
    integer         :: num

    if (num > 0) then; write(ostr%nl_(2:7), 100) num
                 else;       ostr%nl_(2:7) = ' '
    endif
100 format(I5,'/')
  end subroutine

  
!_PROC_EXPORT(ostream_write)
  subroutine ostream_write( ostr, str )
    type(ostream_t)  :: ostr
    character(len=*) :: str

    ! indentation ...
    if (ostr%indents_ > 0 .and. ostr%fuseCnt_ == 0) &
      write( ostr%channel, '(A$)' ) repeat("   ", ostr%indents_)

    ! write given character string
    write( ostr%channel, ostr%fmt_ ) str
    
    ! newlines ...
    if (ostr%fuseCnt_ > 0) then; ostr%fuseCnt_ = ostr%fuseCnt_ - 1
                           else; write( ostr%channel, ostr%nl_ )
    end if
  end subroutine

end module

