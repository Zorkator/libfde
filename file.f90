
#include "adt/itfUtil.fpp"

#define _optArg(var, arg, default)       \
  if (present(arg)) then; var = arg     ;\
                    else; var = default ;\
  end if

#define _optArgPtr(ptr, arg, default)     \
  if (present(arg)) then; ptr => arg     ;\
                    else; ptr => default ;\
  end if


module adt_file
  use adt_exception
  implicit none

  interface fopen
    module procedure fopen_bool_, fopen_autounit_
  end interface

  private :: fopen_bool_, fopen_autounit_

  contains

!_PROC_EXPORT(newunit)
  integer &
  function newunit( unit, min, max )
    integer, optional, intent(out) :: unit
    integer, optional, intent(in)  :: min, max
    integer                        :: unit_, min_, max_, ios
    logical                        :: is_open

    _optArg( min_, min, 7 )
    _optArg( max_, max, 1000 )

    newunit = -1
    do unit_ = min_, max_
      inquire( unit=unit_, opened=is_open, iostat=ios )
      if (ios /= 0)      cycle
      if (.not. is_open) then
        newunit = unit_
        exit
      end if
    end do
    if (present(unit)) unit = newunit
  end function


!_PROC_EXPORT(open_)
  subroutine open_( unit, file, form, status, action, iostat, descr )
    use adt_string
    integer                                         :: unit
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status, action
    integer,          optional,         intent(out) :: iostat
    character(len=*), optional, target, intent(in)  :: descr

    integer                                         :: iostat_
    character(len=512)                              :: buffer

    character(10),                     save, target :: form_std   = 'FORMATTED' &
                                                     , action_std = 'READWRITE' &
                                                     , status_std = 'UNKNOWN'   &
                                                     , descr_std  = 'file'
    character(:),                           pointer :: form_, action_, status_, descr_

    _optArgPtr(form_,   form,   form_std)
    _optArgPtr(status_, status, status_std)
    _optArgPtr(action_, action, action_std)

    close( unit )
    if (lower(status_) == 'scratch') then
      open( unit,            form=form_, status=status_, action=action_, iostat=iostat_, err=10 )
    else                                                               
      open( unit, file=file, form=form_, status=status_, action=action_, iostat=iostat_, err=10 )
    end if

 10 if (present(iostat)) then
      ! user wants iostat ...
      iostat = iostat_
    elseif (iostat_ /= 0) then
      ! we handle error by throwing exception ...
      _optArgPtr(descr_, descr, descr_std)
      write(buffer, 99, err=20) iostat_, trim(descr_), trim(file), trim(form_), trim(status_)
 20   call throw( IOError, buffer )
    end if
 99 format( 'code ', I2, ' on opening ', A, ' ', A, ' in mode (', A, ', ', A, ')' )
  end subroutine


!_PROC_EXPORT(open_jmp)
  subroutine open_jmp( unit, *, file, form, status, action, iostat, descr )
    integer                                         :: unit
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status, action
    integer,          optional,         intent(out) :: iostat
    character(len=*), optional, target, intent(in)  :: descr
    integer                                         :: iostat_

    call open_( unit, file, form, status, action, iostat_, descr )
    if (iostat_ /= 0) then; return 1
                      else; return
    end if
  end subroutine


!_PROC_EXPORT(fopen_bool_)
  logical &
  function fopen_bool_( unit, file, form, status, action, iostat, descr )
    integer                                         :: unit
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status, action
    integer,          optional,         intent(out) :: iostat
    character(len=*), optional, target, intent(in)  :: descr

    call open_( unit, file, form, status, action, iostat, descr ) !< might throw IOError if iostat not given!
    if (present(iostat)) then; fopen_bool_ = (iostat == 0) !< check for success: iostat = 0
                         else; fopen_bool_ = .true.        !< there was no Exception - so it has to be success!
    end if
  end function


!_PROC_EXPORT(fopen_autounit_)
  integer &
  function fopen_autounit_( file, form, status, action, iostat, descr )
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status, action
    integer,          optional,         intent(out) :: iostat
    character(len=*), optional, target, intent(in)  :: descr

    call open_( newunit(fopen_autounit_), file, form, status, action, iostat, descr )
  end function
end module

