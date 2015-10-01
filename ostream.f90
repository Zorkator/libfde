
#include "adt/itfUtil.fpp"

module adt_ostream
  use adt_typeinfo
  implicit none
  private

  type, public :: ostream_t
    private
    procedure(), nopass, pointer :: drainFunc => null()
    integer                      :: channel   =  6
    character(len=10)            :: fmt       =  "(A     $)"
    character(len=10)            :: nl        =  "(     /$)"
    integer                      :: width     =  0
    integer                      :: indents   =  0
    integer                      :: fuseCnt   =  0
  end type

  interface ostream   ; module procedure ostream_create    ; end interface
  interface width     ; module procedure ostream_width     ; end interface
  interface newline   ; module procedure ostream_newline   ; end interface
  interface indent    ; module procedure ostream_indent    ; end interface
  interface fuselines ; module procedure ostream_fuselines ; end interface
  interface set       ; module procedure ostream_set       ; end interface
  interface write     ; module procedure ostream_write     ; end interface
  interface error     ; module procedure ostream_error     ; end interface

  public :: ostream, width, newline, indent, fuselines, set, write, error

  contains

!_PROC_EXPORT(ostream_create)
  pure &
  function ostream_create( chnl ) result(res)
    integer, intent(in) :: chnl
    type(ostream_t)     :: res
    res%drainFunc => ostream_drain_
    res%channel   = chnl
  end function


!_PROC_EXPORT(ostream_width)
  subroutine ostream_width( self, num )
    type(ostream_t) :: self
    integer         :: num

    self%width = -num
    if (num > 0) then; write(self%fmt(3:7), '(I5)') num
                 else;       self%fmt(3:7) = ' '
    endif
  end subroutine


!_PROC_EXPORT(ostream_indent)
  subroutine ostream_indent( self, num )
    type(ostream_t) :: self
    integer         :: num
    self%indents = num 
  end subroutine

  
!_PROC_EXPORT(ostream_fuselines)
  subroutine ostream_fuselines( self, num )
    type(ostream_t) :: self
    integer         :: num

    if (num > 0) then; 
      self%fuseCnt = self%fuseCnt + sign( num, self%fuseCnt )
    else if(self%fuseCnt) then
      write( self%channel, self%nl )
      self%fuseCnt = 0
    end if
  end subroutine

  
!_PROC_EXPORT(ostream_newline)
  subroutine ostream_newline( self, num )
    type(ostream_t) :: self
    integer         :: num

    if (num > 0) then; write(self%nl(2:7), 100) num
                 else;       self%nl(2:7) = ' '
    endif
100 format(I5,'/')
  end subroutine


!_PROC_EXPORT(ostream_set)
  subroutine ostream_set( self, width, indent, fuselines, newline )
    type(ostream_t)   :: self
    integer, optional :: width, indent, fuselines, newline

    if (present(width))     call ostream_width( self, width )
    if (present(indent))    call ostream_indent( self, indent )
    if (present(fuselines)) call ostream_fuselines( self, fuselines )
    if (present(newline))   call ostream_newline( self, newline )
  end subroutine


!_PROC_EXPORT(ostream_write)
  subroutine ostream_write( self, str )
    type(ostream_t)  :: self
    character(len=*) :: str
    call self%drainFunc( self, trim(str) )
  end subroutine

  
  subroutine ostream_drain_( self, str )
    type(ostream_t)  :: self
    character(len=*) :: str
    integer*4        :: padding

    ! write indentation ... only if line fusion is not in effect!
    if (self%indents > 0 .and. self%fuseCnt >= 0) &
      write( self%channel, '(A$)' ) repeat("   ", self%indents)

    ! write given character string
    write( self%channel, self%fmt ) str

    padding = self%width - len(str)
    if (padding > 0) &
      write( self%channel, '(A$)' ) repeat(" ", padding)

    ! check for line fusion ...
    if      (self%fuseCnt > 0) then; self%fuseCnt = -self%fuseCnt    !< start line fusion
    else if (self%fuseCnt < 0) then; self%fuseCnt = self%fuseCnt + 1 !< count line fusion
    end if

    ! ... and write newline[s] if line fusion done or not in effect
    if (self%fuseCnt == 0) &
      write( self%channel, self%nl )
  end subroutine

  
!_PROC_EXPORT(ostream_error)
  subroutine ostream_error( self, ti )
    type(ostream_t)  :: self
    type(TypeInfo_t) :: ti
    write( 0, * ) ">> libadt: can't stream " // trim(ti%typeId)
  end subroutine

end module

