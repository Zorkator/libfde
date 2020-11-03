
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

#include "fde/itfUtil.fpp"


!_PROC_EXPORT(crc32_bytebuffer_c)
  function crc32_bytebuffer_c( seed, buf, size ) result(crc_)
    use iso_c_binding
    integer(kind=c_int32_t), intent(in) :: seed
    integer(kind=c_size_t),  intent(in) :: size
    integer(kind=c_int8_t),  intent(in) :: buf(size)
    integer(kind=c_int32_t)             :: crc_, tabIdx
    integer(kind=c_size_t)              :: idx

    integer(kind=c_int32_t), parameter, dimension(0:255) :: crc32_tab = (/ &
      int(z'00000000'), int(z'77073096'), int(z'ee0e612c'), int(z'990951ba'), int(z'076dc419'), int(z'706af48f'), &
      int(z'e963a535'), int(z'9e6495a3'), int(z'0edb8832'), int(z'79dcb8a4'), int(z'e0d5e91e'), int(z'97d2d988'), &
      int(z'09b64c2b'), int(z'7eb17cbd'), int(z'e7b82d07'), int(z'90bf1d91'), int(z'1db71064'), int(z'6ab020f2'), &
      int(z'f3b97148'), int(z'84be41de'), int(z'1adad47d'), int(z'6ddde4eb'), int(z'f4d4b551'), int(z'83d385c7'), &
      int(z'136c9856'), int(z'646ba8c0'), int(z'fd62f97a'), int(z'8a65c9ec'), int(z'14015c4f'), int(z'63066cd9'), &
      int(z'fa0f3d63'), int(z'8d080df5'), int(z'3b6e20c8'), int(z'4c69105e'), int(z'd56041e4'), int(z'a2677172'), &
      int(z'3c03e4d1'), int(z'4b04d447'), int(z'd20d85fd'), int(z'a50ab56b'), int(z'35b5a8fa'), int(z'42b2986c'), &
      int(z'dbbbc9d6'), int(z'acbcf940'), int(z'32d86ce3'), int(z'45df5c75'), int(z'dcd60dcf'), int(z'abd13d59'), &
      int(z'26d930ac'), int(z'51de003a'), int(z'c8d75180'), int(z'bfd06116'), int(z'21b4f4b5'), int(z'56b3c423'), &
      int(z'cfba9599'), int(z'b8bda50f'), int(z'2802b89e'), int(z'5f058808'), int(z'c60cd9b2'), int(z'b10be924'), &
      int(z'2f6f7c87'), int(z'58684c11'), int(z'c1611dab'), int(z'b6662d3d'), int(z'76dc4190'), int(z'01db7106'), &
      int(z'98d220bc'), int(z'efd5102a'), int(z'71b18589'), int(z'06b6b51f'), int(z'9fbfe4a5'), int(z'e8b8d433'), &
      int(z'7807c9a2'), int(z'0f00f934'), int(z'9609a88e'), int(z'e10e9818'), int(z'7f6a0dbb'), int(z'086d3d2d'), &
      int(z'91646c97'), int(z'e6635c01'), int(z'6b6b51f4'), int(z'1c6c6162'), int(z'856530d8'), int(z'f262004e'), &
      int(z'6c0695ed'), int(z'1b01a57b'), int(z'8208f4c1'), int(z'f50fc457'), int(z'65b0d9c6'), int(z'12b7e950'), &
      int(z'8bbeb8ea'), int(z'fcb9887c'), int(z'62dd1ddf'), int(z'15da2d49'), int(z'8cd37cf3'), int(z'fbd44c65'), &
      int(z'4db26158'), int(z'3ab551ce'), int(z'a3bc0074'), int(z'd4bb30e2'), int(z'4adfa541'), int(z'3dd895d7'), &
      int(z'a4d1c46d'), int(z'd3d6f4fb'), int(z'4369e96a'), int(z'346ed9fc'), int(z'ad678846'), int(z'da60b8d0'), &
      int(z'44042d73'), int(z'33031de5'), int(z'aa0a4c5f'), int(z'dd0d7cc9'), int(z'5005713c'), int(z'270241aa'), &
      int(z'be0b1010'), int(z'c90c2086'), int(z'5768b525'), int(z'206f85b3'), int(z'b966d409'), int(z'ce61e49f'), &
      int(z'5edef90e'), int(z'29d9c998'), int(z'b0d09822'), int(z'c7d7a8b4'), int(z'59b33d17'), int(z'2eb40d81'), &
      int(z'b7bd5c3b'), int(z'c0ba6cad'), int(z'edb88320'), int(z'9abfb3b6'), int(z'03b6e20c'), int(z'74b1d29a'), &
      int(z'ead54739'), int(z'9dd277af'), int(z'04db2615'), int(z'73dc1683'), int(z'e3630b12'), int(z'94643b84'), &
      int(z'0d6d6a3e'), int(z'7a6a5aa8'), int(z'e40ecf0b'), int(z'9309ff9d'), int(z'0a00ae27'), int(z'7d079eb1'), &
      int(z'f00f9344'), int(z'8708a3d2'), int(z'1e01f268'), int(z'6906c2fe'), int(z'f762575d'), int(z'806567cb'), &
      int(z'196c3671'), int(z'6e6b06e7'), int(z'fed41b76'), int(z'89d32be0'), int(z'10da7a5a'), int(z'67dd4acc'), &
      int(z'f9b9df6f'), int(z'8ebeeff9'), int(z'17b7be43'), int(z'60b08ed5'), int(z'd6d6a3e8'), int(z'a1d1937e'), &
      int(z'38d8c2c4'), int(z'4fdff252'), int(z'd1bb67f1'), int(z'a6bc5767'), int(z'3fb506dd'), int(z'48b2364b'), &
      int(z'd80d2bda'), int(z'af0a1b4c'), int(z'36034af6'), int(z'41047a60'), int(z'df60efc3'), int(z'a867df55'), &
      int(z'316e8eef'), int(z'4669be79'), int(z'cb61b38c'), int(z'bc66831a'), int(z'256fd2a0'), int(z'5268e236'), &
      int(z'cc0c7795'), int(z'bb0b4703'), int(z'220216b9'), int(z'5505262f'), int(z'c5ba3bbe'), int(z'b2bd0b28'), &
      int(z'2bb45a92'), int(z'5cb36a04'), int(z'c2d7ffa7'), int(z'b5d0cf31'), int(z'2cd99e8b'), int(z'5bdeae1d'), &
      int(z'9b64c2b0'), int(z'ec63f226'), int(z'756aa39c'), int(z'026d930a'), int(z'9c0906a9'), int(z'eb0e363f'), &
      int(z'72076785'), int(z'05005713'), int(z'95bf4a82'), int(z'e2b87a14'), int(z'7bb12bae'), int(z'0cb61b38'), &
      int(z'92d28e9b'), int(z'e5d5be0d'), int(z'7cdcefb7'), int(z'0bdbdf21'), int(z'86d3d2d4'), int(z'f1d4e242'), &
      int(z'68ddb3f8'), int(z'1fda836e'), int(z'81be16cd'), int(z'f6b9265b'), int(z'6fb077e1'), int(z'18b74777'), &
      int(z'88085ae6'), int(z'ff0f6a70'), int(z'66063bca'), int(z'11010b5c'), int(z'8f659eff'), int(z'f862ae69'), &
      int(z'616bffd3'), int(z'166ccf45'), int(z'a00ae278'), int(z'd70dd2ee'), int(z'4e048354'), int(z'3903b3c2'), &
      int(z'a7672661'), int(z'd06016f7'), int(z'4969474d'), int(z'3e6e77db'), int(z'aed16a4a'), int(z'd9d65adc'), &
      int(z'40df0b66'), int(z'37d83bf0'), int(z'a9bcae53'), int(z'debb9ec5'), int(z'47b2cf7f'), int(z'30b5ffe9'), &
      int(z'bdbdf21c'), int(z'cabac28a'), int(z'53b39330'), int(z'24b4a3a6'), int(z'bad03605'), int(z'cdd70693'), &
      int(z'54de5729'), int(z'23d967bf'), int(z'b3667a2e'), int(z'c4614ab8'), int(z'5d681b02'), int(z'2a6f2b94'), &
      int(z'b40bbe37'), int(z'c30c8ea1'), int(z'5a05df1b'), int(z'2d02ef8d') /)

    crc_ = ieor( seed, int(z'ffffffff') )
    do idx = 1, size
      tabIdx = iand( ieor( crc_, int( buf(idx), c_int32_t ) ), int(z'000000ff') )
      crc_   = ieor( crc32_tab(tabIdx), ishft( crc_, -8 ) )
    end do
    crc_ = ieor( crc_, int(z'ffffffff') )
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
    integer(kind=c_size_t),  intent(in) :: len
    integer(kind=c_int32_t),   optional :: seed, iostat
    integer(kind=c_int32_t)             :: crc_, crc32_bytebuffer_c
    integer(kind=c_int8_t)              :: buf(1024)
    integer(kind=c_int32_t)             :: iostat_, cnt
    integer(kind=c_size_t)              :: block

    _optArg( crc_, seed, 0 )

    iostat_ = 0
    block   = size(buf)
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
    use fde_file, only: fopen, newunit, file_size
    character(len=*),      intent(in) :: fileName
    integer(kind=c_int32_t), optional :: seed, iostat
    integer(kind=c_int32_t)           :: crc_
    integer(kind=c_int32_t)           :: chnl

    interface
      integer(kind=c_int32_t) &
      function crc32_file_channel( chnl, len, seed, iostat )
        use iso_c_binding
        integer(kind=c_int32_t), intent(in) :: chnl
        integer(kind=c_size_t),  intent(in) :: len
        integer(kind=c_int32_t),   optional :: seed, iostat
      end function
    end interface

    chnl = fopen( fileName, form="unformatted", access="stream", status="old", action="read")
    crc_ = crc32_file_channel( chnl, file_size(chnl), seed, iostat )
    close( chnl )
  end function

