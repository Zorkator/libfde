
#include "fde/itfUtil.fpp"

module fde_file
   use iso_c_binding
   use fde_exception
   implicit none

   interface fopen
      module procedure fopen_bool_, fopen_bool_v, fopen_autounit_, fopen_autounit_v
   end interface

   interface fclose
      module procedure fclose_bool_, fclose_bool_v
   end interface

   interface file_exists
      module procedure file_exists_, file_exists_v
   end interface

   interface file_size
      module procedure file_size_name, file_size_unit
   end interface

   interface file_opened
      module procedure file_opened_name, file_opened_unit
   end interface

   interface open
      module procedure open_, open_v
   end interface

   interface close
      module procedure close_, close_v
   end interface

   private :: fopen_bool_, fopen_autounit_

   character(12), private, target :: access_opts(3)   = ['SEQUENTIAL ', 'DIRECT     ', 'TRANSPARENT']
   character(12), private, target :: action_opts(3)   = ['READWRITE  ', 'READ       ', 'WRITE      ']
   character(12), private, target :: form_opts(3)     = ['FORMATTED  ', 'UNFORMATED ', 'BINARY     ']
   character(12), private, target :: position_opts(3) = ['ASIS       ', 'REWIND     ', 'APPEND     ']
   character(12), private, target :: status_opts(5)   = ['UNKNOWN', 'NEW    ', 'REPLACE', 'SCRATCH', 'OLD    ']
   character(12), private, target :: descr_opts(1)    = ['file']

   character(16), private, target :: errBase_opts(2)  = ['opening         ', 'refuse reopening']

#  define _open_kwArgs_list    access, action, form, position, status, descr

contains

!_PROC_EXPORT(file_exists_)
   logical &
   function file_exists_( file, iostat ) result(res)
      character(len=*),           intent(in)  :: file
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( file=file, exist=res, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
   end function


!_PROC_EXPORT(file_exists_v)
   function file_exists_v( file, iostat ) result(res)
      character(len=*),           intent(in)  :: file(:)
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_, i
      logical                                 :: res(size(file))

      do i = 1, size(file)
         inquire( file=file(i), exist=res(i), iostat=iostat_ )
         if (present(iostat)) iostat = max( iostat, iostat_ )
      end do
   end function


!_PROC_EXPORT(file_size_name)
   integer(c_size_t) &
   function file_size_name( file, iostat ) result(res)
      character(len=*),           intent(in)  :: file
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( file=file, size=res, iostat=iostat_ )
      if (present(iostat)) iostat = iostat_
   end function


!_PROC_EXPORT(file_size_unit)
   integer(c_size_t) &
   function file_size_unit( unit, iostat ) result(res)
      integer,                    intent(in)  :: unit
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      inquire( unit=unit, size=res, iostat=iostat_ )
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
   subroutine open_( unit, file, _open_kwArgs_list, recl, iostat )
      _use_if_INTEL(ifcore)
      use fde_string
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: _open_kwArgs_list
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      integer                                         :: iostat_
      character(len=512)                              :: buffer
      character(:),                           pointer :: access_, action_, form_, position_, status_, descr_, errBase_

      ! Set pointer to either given string argument or the default string ...
      _optArgPtr( access_,   access,   access_opts(1) )
      _optArgPtr( action_,   action,   action_opts(1) )
      ! CAUTION: The default of FORM depends on chosen ACCESS!!
      !          Thus, for FORM we use the same index where we find the chosen ACCESS in access_opts
      _optArgPtr( form_,     form,     form_opts( indexOf(upper(access_), access_opts, 1) ) )
      _optArgPtr( position_, position, position_opts(1) )
      _optArgPtr( status_,   status,   status_opts(1) )
      _optArgPtr( descr_,    descr,    descr_opts(1) )

      if (file_opened( unit )) then
         errBase_ => errBase_opts(2)
         iostat_  =  34
      else
         errBase_ => errBase_opts(1)

#        define _open_kwArgs_pass   access=access_, action=action_, form=form_, position=position_, status=status_

         if (upper(status_) == 'SCRATCH') then
            if (present(recl)) then; open( unit,            _open_kwArgs_pass, recl=recl, iostat=iostat_, err=10 )
                               else; open( unit,            _open_kwArgs_pass,            iostat=iostat_, err=10 )
            end if
         else
            if (present(recl)) then; open( unit, file=file, _open_kwArgs_pass, recl=recl, iostat=iostat_, err=10 )
                               else; open( unit, file=file, _open_kwArgs_pass,            iostat=iostat_, err=10 )
            end if
         end if
      end if

      10 if (present(iostat)) then
         ! caller just wants iostat ...
         iostat = iostat_

      elseif (iostat_ /= 0) then
         ! otherwise we handle error by throwing an exception ...
         write(buffer, 100, err=20) iostat_, trim(errBase_), trim(descr_), trim(file)
         call gerror( buffer(len_trim(buffer)+2:) )
         20 call throw( IOError, buffer )
      end if

      100 format( 'code ', I3, ', ', A, ' ', A, ' "', A, '": ' )

   contains

      integer &
      function indexOf( what, array, default ) result(idx)
         character(len=*),               intent(in) :: what
         character(len=*), dimension(:), intent(in) :: array
         integer                                    :: default
         do idx = 1, size(array)
            if (what == array(idx)) return
         end do
         idx = default
      end function
   end subroutine


!_PROC_EXPORT(open_v)
   subroutine open_v( units, files, _open_kwArgs_list, recl, iostat )
      integer,          dimension(:),           intent(in) :: units
      character(len=*), dimension(size(units)), intent(in) :: files
      character(len=*), optional,       target, intent(in) :: _open_kwArgs_list
      integer,          optional,               intent(in) :: recl
      integer,          optional,              intent(out) :: iostat
      integer                                              :: i

      do i = 1, size(units)
         call open( units(i), files(i), _open_kwArgs_list, recl, iostat )
      end do
   end subroutine


!_PROC_EXPORT(open_jmp)
   subroutine open_jmp( unit, *, file, _open_kwArgs_list, recl, iostat )
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: _open_kwArgs_list
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      integer                                         :: iostat_

      call open_( unit, file, _open_kwArgs_list, recl, iostat_ )
      if (present(iostat)) iostat = iostat_
      if (iostat_ /= 0) then; return 1
                        else; return
      end if
   end subroutine


!_PROC_EXPORT(fopen_bool_)
   logical &
   function fopen_bool_( unit, file, _open_kwArgs_list, recl ) result(res)
      integer,                            intent(in)  :: unit
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: _open_kwArgs_list
      integer,          optional,         intent(in)  :: recl
      integer                                         :: iostat_

      call open_( unit, file, _open_kwArgs_list, recl, iostat=iostat_ )
      res = (iostat_ == 0) !< check for success
   end function


!_PROC_EXPORT(fopen_bool_v)
   function fopen_bool_v( units, files, _open_kwArgs_list, recl ) result(res)
      integer,          dimension(:),           intent(in)  :: units
      character(len=*), dimension(size(units)), intent(in)  :: files
      character(len=*), optional, target,       intent(in)  :: _open_kwArgs_list
      integer,          optional,               intent(in)  :: recl
      logical,          dimension(size(files))              :: res
      integer                                               :: i

      do i = 1, size(files)
         res(i) = fopen( units(i), files(i), _open_kwArgs_list, recl )
      end do
   end function


!_PROC_EXPORT(fopen_autounit_)
   integer &
   function fopen_autounit_( file, _open_kwArgs_list, recl, iostat ) result(res)
      character(len=*),                   intent(in)  :: file
      character(len=*), optional, target, intent(in)  :: _open_kwArgs_list
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat

      call open_( newunit(res), file, _open_kwArgs_list, recl, iostat )
   end function


!_PROC_EXPORT(fopen_autounit_v)
   function fopen_autounit_v( files, _open_kwArgs_list, recl, iostat ) result(res)
      character(len=*), dimension(:),     intent(in)  :: files
      character(len=*), optional, target, intent(in)  :: _open_kwArgs_list
      integer,          optional,         intent(in)  :: recl
      integer,          optional,         intent(out) :: iostat
      integer,          dimension(size(files))        :: res
      integer                                         :: i

      do i = 1, size(files)
         res(i) = fopen( files(i), _open_kwArgs_list, recl, iostat )
      end do
   end function


!_PROC_EXPORT(close_)
   subroutine close_( unit, status, descr, iostat )
      _use_if_INTEL(ifcore)
      integer,                            intent(in)  :: unit
      character(len=*), optional, target, intent(in)  :: status, descr
      integer,          optional,         intent(out) :: iostat
      integer                                         :: iostat_
      character(len=512)                              :: buffer
      character(len=256)                              :: file
      character(:),                           pointer :: descr_

      if (present(status)) then; close( unit, status=status, iostat=iostat_, err=10 )
                           else; close( unit,                iostat=iostat_, err=10 )
      end if

      10 if (present(iostat)) then
         ! caller wants iostat ...
         iostat = iostat_

      elseif (iostat_ /= 0) then
         ! otherwise we handle error by throwing an exception ...
         _optArgPtr(descr_, descr, descr_opts(1))
         write(buffer, 100, err=20) iostat_, trim(descr_), file_name(unit, file)
         call gerror( buffer(len_trim(buffer)+2:) )
         20 call throw( IOError, buffer )
      end if

      100 format( 'code ', I3, ', on closing ', A, ' "', A, '": ' )
   end subroutine


!_PROC_EXPORT(close_v)
   subroutine close_v( units, status, descr, iostat )
      integer,          dimension(:), intent(in)  :: units
      character(len=*), optional,     intent(in)  :: status, descr
      integer,          optional,     intent(out) :: iostat
      integer                                     :: i

      do i = 1, size(units)
         call close_( units(i), status, descr, iostat )
      end do
   end subroutine


!_PROC_EXPORT(close_jmp)
   subroutine close_jmp( unit, *, status, descr, iostat )
      integer,                    intent(in)  :: unit
      character(len=*), optional, intent(in)  :: status, descr
      integer,          optional, intent(out) :: iostat
      integer                                 :: iostat_

      call close_( unit, status, descr, iostat_ )
      if (present(iostat)) iostat = iostat_
      if (iostat_ /= 0) then; return 1
                        else; return
      end if
   end subroutine


!_PROC_EXPORT(fclose_bool_)
   logical &
   function fclose_bool_( unit, status, descr ) result(res)
      integer,                    intent(in)  :: unit
      character(len=*), optional, intent(in)  :: status, descr
      integer                                 :: iostat_

      call close_( unit, status, descr, iostat=iostat_ )
      res = (iostat_ == 0) !< check for success
   end function


!_PROC_EXPORT(fclose_bool_v)
   function fclose_bool_v( units, status, descr ) result(res)
      integer,          dimension(:), intent(in)  :: units
      character(len=*), optional,     intent(in)  :: status, descr
      logical,          dimension(size(units))    :: res
      integer                                     :: i

      do i = 1, size(units)
         res(i) = fclose( units(i), status, descr )
      end do
   end function

end module

