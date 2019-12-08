
module fde_crc
    use iso_c_binding
    implicit none
    private

    interface crc32
        function crc32_bytebuffer_c( seed, buf, size ) result(res)
            import
            integer(kind=c_int32_t), intent(in) :: seed
            integer(kind=c_size_t),  intent(in) :: size
            integer(kind=c_int8_t),  intent(in) :: buf(size)
            integer(kind=c_int32_t)             :: res
        end function

        function crc32_string_c( str ) result(res)
            import
            character(len=*), intent(in) :: str
            integer(kind=c_int32_t)      :: res
        end function

        function crc32_c_ptr( cptr, size ) result(res)
            import
            type (c_ptr),           intent(in) :: cptr
            integer(kind=c_size_t), intent(in) :: size
            integer(kind=c_int32_t)            :: res
        end function

        module procedure crc32_c_ptr_i32
    end interface


    interface crc32_file
        function crc32_file_channel( chnl, len, seed, iostat ) result(res)
            import
            integer(kind=c_int32_t), intent(in) :: chnl
            integer(kind=c_size_t),  intent(in) :: len
            integer(kind=c_int32_t),   optional :: seed, iostat
            integer(kind=c_int32_t)             :: res
        end function

        function crc32_file_name( fileName, iostat, seed ) result(res)
            import
            character(len=*),      intent(in) :: fileName
            integer(kind=c_int32_t), optional :: seed, iostat
            integer(kind=c_int32_t)           :: res
        end function
    end interface

    public :: crc32, crc32_file

contains

    function crc32_c_ptr_i32( cptr, size ) result(res)
        type (c_ptr),            intent(in) :: cptr
        integer(kind=c_int32_t), intent(in) :: size
        integer(kind=c_int32_t)             :: res
        res = crc32( cptr, int(size, c_size_t) )
    end function

    function crc32_file_channel_i32( chnl, len, seed, iostat ) result(res)
        integer(kind=c_int32_t), intent(in) :: chnl
        integer(kind=c_size_t),  intent(in) :: len
        integer(kind=c_int32_t),   optional :: seed, iostat
        integer(kind=c_int32_t)             :: res
        res = crc32_file( chnl, int(len, c_size_t), seed, iostat )
    end function
end module

