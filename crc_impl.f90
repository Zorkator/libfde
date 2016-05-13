
!!
!!  COPYRIGHT (C) 1986 Gary S. Brown.  You may use this program, or
!!  code or tables extracted from it, as desired without restriction.
!!
!!  First, the polynomial itself and its table of feedback terms.  The
!!  polynomial is
!!  X^32+X^26+X^23+X^22+X^16+X^12+X^11+X^10+X^8+X^7+X^5+X^4+X^2+X^1+X^0
!!
!!  Note that we take it "backwards" and put the highest-order term in
!!  the lowest-order bit.  The X^32 term is "implied"; the LSB is the
!!  X^31 term, etc.  The X^0 term (usually shown as "+1") results in
!!  the MSB being 1
!!
!!  Note that the usual hardware shift register implementation, which
!!  is what we're using (we're merely optimizing it by doing eight-bit
!!  chunks at a time) shifts bits into the lowest-order term.  In our
!!  implementation, that means shifting towards the right.  Why do we
!!  do it this way?  Because the calculated CRC must be transmitted in
!!  order from highest-order term to lowest-order term.  UARTs transmit
!!  characters in order from LSB to MSB.  By storing the CRC this way
!!  we hand it to the UART in the order low-byte to high-byte; the UART
!!  sends each low-bit to hight-bit; and the result is transmission bit
!!  by bit from highest- to lowest-order term without requiring any bit
!!  shuffling on our part.  Reception works similarly
!!
!!  The feedback terms table consists of 256, 32-bit entries.  Notes
!!
!!      The table can be generated at runtime if desired; code to do so
!!      is shown later.  It might not be obvious, but the feedback
!!      terms simply represent the results of eight shift/xor opera
!!      tions for all combinations of data and CRC register values
!!
!!      The values must be right-shifted by eight bits by the "updcrc
!!      logic; the shift must be unsigned (bring in zeroes).  On some
!!      hardware you could probably optimize the shift in assembler by
!!      using byte-swap instructions
!!      polynomial $edb88320
!!
!!
!! CRC32 code derived from work by Gary S. Brown.
!!

#include "adt/itfUtil.fpp"


!_PROC_EXPORT(crc32_bytebuffer_c)
  function crc32_bytebuffer_c( seed, buf, size ) result(crc_)
    use iso_c_binding
    integer(kind=c_int32_t), intent(in) :: seed
    integer(kind=c_size_t),  intent(in) :: size
    integer(kind=c_int8_t),  intent(in) :: buf(size)
    integer(kind=c_int32_t)             :: crc_, tabIdx
    integer(kind=c_size_t)              :: idx
  
    integer(kind=c_int32_t), parameter, dimension(0:255) :: crc32_tab = (/ &
      z'00000000', z'77073096', z'ee0e612c', z'990951ba', z'076dc419', z'706af48f', &
      z'e963a535', z'9e6495a3', z'0edb8832', z'79dcb8a4', z'e0d5e91e', z'97d2d988', &
      z'09b64c2b', z'7eb17cbd', z'e7b82d07', z'90bf1d91', z'1db71064', z'6ab020f2', &
      z'f3b97148', z'84be41de', z'1adad47d', z'6ddde4eb', z'f4d4b551', z'83d385c7', &
      z'136c9856', z'646ba8c0', z'fd62f97a', z'8a65c9ec', z'14015c4f', z'63066cd9', &
      z'fa0f3d63', z'8d080df5', z'3b6e20c8', z'4c69105e', z'd56041e4', z'a2677172', &
      z'3c03e4d1', z'4b04d447', z'd20d85fd', z'a50ab56b', z'35b5a8fa', z'42b2986c', &
      z'dbbbc9d6', z'acbcf940', z'32d86ce3', z'45df5c75', z'dcd60dcf', z'abd13d59', &
      z'26d930ac', z'51de003a', z'c8d75180', z'bfd06116', z'21b4f4b5', z'56b3c423', &
      z'cfba9599', z'b8bda50f', z'2802b89e', z'5f058808', z'c60cd9b2', z'b10be924', &
      z'2f6f7c87', z'58684c11', z'c1611dab', z'b6662d3d', z'76dc4190', z'01db7106', &
      z'98d220bc', z'efd5102a', z'71b18589', z'06b6b51f', z'9fbfe4a5', z'e8b8d433', &
      z'7807c9a2', z'0f00f934', z'9609a88e', z'e10e9818', z'7f6a0dbb', z'086d3d2d', &
      z'91646c97', z'e6635c01', z'6b6b51f4', z'1c6c6162', z'856530d8', z'f262004e', &
      z'6c0695ed', z'1b01a57b', z'8208f4c1', z'f50fc457', z'65b0d9c6', z'12b7e950', &
      z'8bbeb8ea', z'fcb9887c', z'62dd1ddf', z'15da2d49', z'8cd37cf3', z'fbd44c65', &
      z'4db26158', z'3ab551ce', z'a3bc0074', z'd4bb30e2', z'4adfa541', z'3dd895d7', &
      z'a4d1c46d', z'd3d6f4fb', z'4369e96a', z'346ed9fc', z'ad678846', z'da60b8d0', &
      z'44042d73', z'33031de5', z'aa0a4c5f', z'dd0d7cc9', z'5005713c', z'270241aa', &
      z'be0b1010', z'c90c2086', z'5768b525', z'206f85b3', z'b966d409', z'ce61e49f', &
      z'5edef90e', z'29d9c998', z'b0d09822', z'c7d7a8b4', z'59b33d17', z'2eb40d81', &
      z'b7bd5c3b', z'c0ba6cad', z'edb88320', z'9abfb3b6', z'03b6e20c', z'74b1d29a', &
      z'ead54739', z'9dd277af', z'04db2615', z'73dc1683', z'e3630b12', z'94643b84', &
      z'0d6d6a3e', z'7a6a5aa8', z'e40ecf0b', z'9309ff9d', z'0a00ae27', z'7d079eb1', &
      z'f00f9344', z'8708a3d2', z'1e01f268', z'6906c2fe', z'f762575d', z'806567cb', &
      z'196c3671', z'6e6b06e7', z'fed41b76', z'89d32be0', z'10da7a5a', z'67dd4acc', &
      z'f9b9df6f', z'8ebeeff9', z'17b7be43', z'60b08ed5', z'd6d6a3e8', z'a1d1937e', &
      z'38d8c2c4', z'4fdff252', z'd1bb67f1', z'a6bc5767', z'3fb506dd', z'48b2364b', &
      z'd80d2bda', z'af0a1b4c', z'36034af6', z'41047a60', z'df60efc3', z'a867df55', &
      z'316e8eef', z'4669be79', z'cb61b38c', z'bc66831a', z'256fd2a0', z'5268e236', &
      z'cc0c7795', z'bb0b4703', z'220216b9', z'5505262f', z'c5ba3bbe', z'b2bd0b28', &
      z'2bb45a92', z'5cb36a04', z'c2d7ffa7', z'b5d0cf31', z'2cd99e8b', z'5bdeae1d', &
      z'9b64c2b0', z'ec63f226', z'756aa39c', z'026d930a', z'9c0906a9', z'eb0e363f', &
      z'72076785', z'05005713', z'95bf4a82', z'e2b87a14', z'7bb12bae', z'0cb61b38', &
      z'92d28e9b', z'e5d5be0d', z'7cdcefb7', z'0bdbdf21', z'86d3d2d4', z'f1d4e242', &
      z'68ddb3f8', z'1fda836e', z'81be16cd', z'f6b9265b', z'6fb077e1', z'18b74777', &
      z'88085ae6', z'ff0f6a70', z'66063bca', z'11010b5c', z'8f659eff', z'f862ae69', &
      z'616bffd3', z'166ccf45', z'a00ae278', z'd70dd2ee', z'4e048354', z'3903b3c2', &
      z'a7672661', z'd06016f7', z'4969474d', z'3e6e77db', z'aed16a4a', z'd9d65adc', &
      z'40df0b66', z'37d83bf0', z'a9bcae53', z'debb9ec5', z'47b2cf7f', z'30b5ffe9', &
      z'bdbdf21c', z'cabac28a', z'53b39330', z'24b4a3a6', z'bad03605', z'cdd70693', &
      z'54de5729', z'23d967bf', z'b3667a2e', z'c4614ab8', z'5d681b02', z'2a6f2b94', &
      z'b40bbe37', z'c30c8ea1', z'5a05df1b', z'2d02ef8d' /)

    crc_ = ieor( seed, z'ffffffff' )
    do idx = 1, size
      tabIdx = iand( ieor( crc_, buf(idx) ), z'000000ff' )
      crc_   = ieor( crc32_tab(tabIdx), ishft( crc_, -8 ) )
    end do
    crc_ = ieor( crc_, z'ffffffff' )
  end function


!_PROC_EXPORT(crc32_string_c)
  function crc32_string_c( str ) result(crc_)
    use iso_c_binding
    character(len=*),          target, intent(in) :: str
    integer(kind=c_int32_t)                       :: crc_, crc32_bytebuffer_c
    integer(kind=c_int8_t), dimension(:), pointer :: buf
    call c_f_pointer( c_loc(str(1:1)), buf, (/len_trim(str)/) )
    crc_ = crc32_bytebuffer_c( 0, buf, int(size(buf), c_size_t) )
  end function


!_PROC_EXPORT(crc32_c_ptr)
  function crc32_c_ptr( cptr, size ) result(crc_)
    use iso_c_binding
    type (c_ptr),                      intent(in) :: cptr
    integer(kind=c_size_t),            intent(in) :: size
    integer(kind=c_int32_t)                       :: crc_, crc32_bytebuffer_c
    integer(kind=c_int8_t), dimension(:), pointer :: buf
    call c_f_pointer( cptr, buf, (/size/) )
    crc_ = crc32_bytebuffer_c( 0, buf, size )
  end function


!_PROC_EXPORT(crc32_file_channel)
  function crc32_file_channel( chnl, len, seed, iostat ) result(crc_)
    use iso_c_binding
    integer(kind=c_int32_t), intent(in) :: chnl
    integer(kind=c_size_t)              :: len
    integer(kind=c_int32_t),   optional :: seed, iostat
    integer(kind=c_int32_t)             :: crc_, crc32_bytebuffer_c
    integer(kind=c_int8_t)              :: buf(1024)
    integer(kind=c_int32_t)             :: iostat_, cnt
    integer(kind=c_size_t)              :: block

    if (present(seed)) then; crc_ = seed
                       else; crc_ = 0
    end if

    block = size(buf)
    do cnt = 1, len/block
      read(chnl, iostat=iostat_) buf(1:block)
      if (iostat_ == 0) then; crc_ = crc32_bytebuffer_c( crc_, buf, block )
                        else; exit
      end if
    end do

    block = mod(len, size(buf))
    if (block > 0 .and. iostat_ == 0) then
      read(chnl, iostat=iostat_) buf(1:block)
      if (iostat_ == 0) then; crc_ = crc32_bytebuffer_c( crc_, buf, block )
      end if
    end if
    if (present(iostat)) iostat = iostat_
  end function


!_PROC_EXPORT(crc32_file_name)
  function crc32_file_name( fileName, iostat, seed ) result(crc_)
    use iso_c_binding
    character(len=*),      intent(in) :: fileName
    integer(kind=c_int32_t), optional :: seed, iostat
    integer(kind=c_int32_t)           :: crc_, crc32_file_channel
    integer(kind=c_int32_t)           :: chnl, iostat_
    integer(kind=c_size_t)            :: len

    open( newunit=chnl, file=fileName, form="unformatted", access="stream", status='old', iostat=iostat_ )
    if (iostat_ == 0) then
      inquire( unit=chnl, size=len )
      crc_ = crc32_file_channel( chnl, len, seed, iostat_ )
      close( chnl )
    end if
    if (present(iostat)) iostat = iostat_
  end function

