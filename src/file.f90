
#include "fde/itfUtil.fpp"

module fde_file
   use fde_exception
   implicit none

   interface fopen
      module procedure fopen_bool_, fopen_autounit_
   end interface

   interface file_opened
      module procedure file_opened_name, file_opened_unit
   end interface

   private :: fopen_bool_, fopen_autounit_

contains

!_PROC_EXPORT(file_exists)
   logical &
   function file_exists( file, iostat ) result(res)
      character(len=*),           intent(in)  :: file
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( file=file, exist=res, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
   end function


!_PROC_EXPORT(file_opened_name)
   logical &
   function file_opened_name( file, iostat ) result(res)
      character(len=*),           intent(in)  :: file
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( file=file, opened=res, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
   end function


!_PROC_EXPORT(file_opened_unit)
   logical &
   function file_opened_unit( unit, iostat ) result(res)
      integer,                    intent(in)  :: unit
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( unit=unit, opened=res, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
   end function


!_PROC_EXPORT(file_name)
   function file_name( unit, fileBuff, iostat ) result(res)
      integer,                    intent(in)  :: unit
      character(len=*), target,   intent(out) :: fileBuff
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_
      character(len=:),              pointer  :: res

      inquire( unit=unit, name=fileBuff, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
      res => fileBuff( : len_trim( fileBuff ) )
   end function


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
   subroutine open_( unit, file, form, status, action, access, recl, iostat, descr )
      use fde_string
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: form, status, action, access
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr

      integer                                         :: iostat_
      character(len=512)                              :: buffer

      character(10),                     save, target :: form_std   = 'FORMATTED'  &
                                                       , action_std = 'READWRITE'  &
                                                       , access_std = 'SEQUENTIAL' &
                                                       , status_std = 'UNKNOWN'    &
                                                       , descr_std  = 'file'
      character(:),                           pointer :: form_, action_, access_, status_, descr_
      integer                                         :: recl_
      logical                                         :: reopened

      _optArgPtr(form_,   form,   form_std)
      _optArgPtr(status_, status, status_std)
      _optArgPtr(action_, action, action_std)
      _optArgPtr(access_, access, access_std)

      _optArg(recl_,   recl,   1)

      reopened = file_opened( unit )
      if (.not. reopened) then
         if (lower(status_) == 'scratch') then
            open( unit,            form=form_, status=status_, action=action_, access=access_, recl=recl_, iostat=iostat_, err=10 )
         else
            open( unit, file=file, form=form_, status=status_, action=action_, access=access_, recl=recl_, iostat=iostat_, err=10 )
         end if
      else
         iostat_ = 14 !< there's only a generic open error code :-/
      end if

      10 if (present(iostat)) then
         ! caller wants iostat ...
         iostat = iostat_
      elseif (iostat_ /= 0) then
         ! we handle error by throwing exception ...
         _optArgPtr(descr_, descr, descr_std)
         if (reopened) then; write(buffer, 100, err=20) iostat_, trim(descr_), trim(file), unit, trim(form_), trim(status_)
                       else; write(buffer, 200, err=20) iostat_, trim(descr_), trim(file), unit, trim(form_), trim(status_)
         end if

         20 call throw( IOError, buffer )
      end if

      100 format( 'code ', I2, ', refuse reopening ', A, ' "', A, '" on channel ', I2, ' in mode (', A, ', ', A, ')' )
      200 format( 'code ', I2, ', opening ',          A, ' "', A, '" on channel ', I2, ' in mode (', A, ', ', A, ')' )
   end subroutine


!_PROC_EXPORT(open_jmp)
   subroutine open_jmp( unit, *, file, form, status, action, access, recl, iostat, descr )
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: form, status, action, access
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr
      integer                                         :: iostat_

      call open_( unit, file, form, status, action, access, recl, iostat_, descr )
      if (present(iostat)) iostat = iostat_
      if (iostat_ /= 0) then; return 1
                        else; return
      end if
   end subroutine


!_PROC_EXPORT(fopen_bool_)
   logical &
   function fopen_bool_( unit, file, form, status, action, access, recl, iostat, descr ) result(res)
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: form, status, action, access
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr

      call open_( unit, file, form, status, action, access, recl, iostat, descr ) !< might throw IOError if iostat not given!
      if (present(iostat)) then; res = (iostat == 0) !< check for success: iostat = 0
                           else; res = .true.        !< there was no Exception - so it has to be success!
      end if
   end function


!_PROC_EXPORT(fopen_autounit_)
   integer &
   function fopen_autounit_( file, form, status, action, access, recl, iostat, descr ) result(res)
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: form, status, action, access
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr

      call open_( newunit(res), file, form, status, action, access, recl, iostat, descr )
   end function


!_PROC_EXPORT(close_)
   subroutine close_( unit, status, iostat, descr )
      integer,                            intent(in)  :: unit
      character(len=*), optional, target, intent(in)  :: status
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr

      integer                                         :: iostat_
      character(len=512)                              :: buffer
      character(len=256)                              :: file

      character(10),                     save, target :: status_std = 'KEEP' &
                                                       , descr_std  = 'file'
      character(:),                           pointer :: status_, descr_

      _optArgPtr(status_, status, status_std)

      close( unit, status=status_, iostat=iostat_, err=10 )

      10 if (present(iostat)) then
         ! caller wants iostat ...
         iostat = iostat_
      elseif (iostat_ /= 0) then
         ! we handle error by throwing exception ...
         _optArgPtr(descr_, descr, descr_std)
         write(buffer, 99, err=20) iostat_, trim(descr_), file_name(unit, file), trim(status_)

         20 call throw( IOError, buffer )
      end if

      99 format( 'code ', I2, ' on closing ', A, ' "', A, '" with status ', A )
   end subroutine


!_PROC_EXPORT(close_jmp)
   subroutine close_jmp( unit, *, status, iostat, descr )
      integer,                            intent(in)  :: unit
      character(len=*), optional, target, intent(in)  :: status
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr
      integer                                         :: iostat_

      call close_( unit, status, iostat_, descr )
      if (present(iostat)) iostat = iostat_
      if (iostat_ /= 0) then; return 1
                        else; return
      end if
   end subroutine


!_PROC_EXPORT(fclose)
   logical &
   function fclose( unit, status, iostat, descr ) result(res)
      integer,                            intent(in)  :: unit
      character(len=*), optional, target, intent(in)  :: status
      integer,          optional,         intent(out) :: iostat
      character(len=*), optional, target, intent(in)  :: descr

      call close_( unit, status, iostat, descr )     !< might throw IOError if iostat not given!
      if (present(iostat)) then; res = (iostat == 0) !< check for success: iostat = 0
                           else; res = .true.        !< there was no Exception - so it has to be success!
      end if
   end function

end module

