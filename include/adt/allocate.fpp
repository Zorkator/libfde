#ifndef __ADT_ALLOCATE_FPP
#define __ADT_ALLOCATE_FPP

#include "adt/string.fpp"

# define __allocate_MemoryError( sym, shape ) \
      call throw( MemoryError, 'The allocation of ' // __sym2str__(sym) // ' with shape ' // __sym2str__(shape) // ' failed' )


# define __allocate_chk_allocated( sym, shape ) \
      if (.not. allocated(sym)) then           ;\
        __allocate_MemoryError( sym, shape )   ;\
      end if


# define _ALLOCATE_st( sym, shape, _st ) \
      allocate( sym shape, stat=_st )


# define _checked_ALLOCATE_st( sym, shape, _st ) \
      select case (0); case default             ;\
        _ALLOCATE_st( sym, shape, _st )         ;\
        __allocate_chk_allocated( sym, shape )  ;\
      end select


# define _assert_ALLOCATE_st( sym, shape, _st ) \
      if (.not. allocated(sym)) then           ;\
        _ALLOCATE_st( sym, shape, _st )        ;\
      end if


# define _assert_checked_ALLOCATE_st( sym, shape, _st ) \
      select case (0); case default                    ;\
        _assert_ALLOCATE_st( sym, shape, _st )         ;\
        __allocate_chk_allocated( sym, shape )         ;\
      end select                                       ;\


# define _DEALLOCATE_st( sym, _st ) \
      deallocate( sym, stat=_st )


# define _REALLOCATE_st( sym, shape, _st )  \
      select case (0); case default        ;\
        _DEALLOCATE_st( sym, _st )         ;\
        _ALLOCATE_st( sym, shape, _st )    ;\
      end select


# define _copied_REALLOCATE_st( sym, shape_, pad, tmp, _st )               \
      select case (0); case default                                     ;\
        call move_alloc( sym, tmp )                                     ;\
        _checked_ALLOCATE_st( sym, shape_, _st )                        ;\
        if (allocated(tmp)) then; sym = reshape( tmp, shape(sym), pad ) ;\
                            else; sym = reshape( pad, shape(sym), pad ) ;\
        end if                                                          ;\
        _DEALLOCATE_st( tmp, _st )                                      ;\
      end select


! The following definition might be changed by the native code using libadt.
!  __istat__: the integer status variable used by the [DE]ALLOCATE macros below.

# if !defined __istat__
#   define __istat__        istat
# endif

# define _ALLOCATE( sym, shape ) \
    _ALLOCATE_st( sym, shape, __istat__ )

# define _checked_ALLOCATE( sym, shape ) \
    _checked_ALLOCATE_st( sym, shape, __istat__ )

# define _assert_ALLOCATE( sym, shape ) \
    _assert_ALLOCATE_st( sym, shape, __istat__ )

# define _assert_checked_ALLOCATE( sym, shape ) \
    _assert_checked_ALLOCATE_st( sym, shape, __istat__ )

# define _DEALLOCATE( sym ) \
    _DEALLOCATE_st( sym, __istat__ )

# define _REALLOCATE( sym, shape ) \
    _REALLOCATE_st( sym, shape, __istat__ )

# define _copied_REALLOCATE( sym, shape, pad, tmp ) \
    _copied_REALLOCATE_st( sym, shape, pad, tmp, __istat__ )

#endif 
! ^^ __ADT_ALLOCATE_FPP

