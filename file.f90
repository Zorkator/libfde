
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
  
  contains
  
  subroutine open_( chnl, file, form, status, iostat, descr )
    integer                                         :: chnl
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status
    integer,          optional,         intent(out) :: iostat
    character(len=*), optional, target, intent(in)  :: descr
  
    integer                                         :: iostat_
    character(len=512)                              :: buffer
  
    character(10),                     save, target :: form_std   = 'FORMATTED' &
                                                     , status_std = 'UNKNOWN'   &
                                                     , descr_std  = 'file'
    character(:),                           pointer :: form_, status_, descr_
  
    _optArgPtr(form_,   form,   form_std)
    _optArgPtr(status_, status, status_std)
  
    close( chnl )
    open( chnl, file=file, form=form_, status=status_, iostat=iostat_, err=10 )
 10 if  (present(iostat)) then
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
  
  
  subroutine open_jmp( chnl, *, file, form, status, iostat )
    integer                                         :: chnl
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status
    integer,          optional,         intent(out) :: iostat
    integer                                         :: iostat_
  
    call open_( chnl, file, form, status, iostat_ )
    if (iostat_ /= 0) then; return 1
                      else; return
    end if
  end subroutine
  
  
  logical &
  function fopen( chnl, file, form, status, iostat )
    integer                                         :: chnl
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status
    integer,          optional,         intent(out) :: iostat
    integer                                         :: iostat_
    
    call open_( chnl, file, form, status, iostat_ )
    fopen = (iostat_ == 0)
  end function
  
  
  integer &
  function fopen_stat( chnl, file, form, status, iostat )
    integer                                         :: chnl
    character(len=*),                   intent(in)  :: file
    character(len=*), optional, target, intent(in)  :: form, status
    integer,          optional,         intent(out) :: iostat
    
    call open_( chnl, file, form, status, fopen_stat )
  end function
end module

