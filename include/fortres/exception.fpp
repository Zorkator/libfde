#ifndef __FORTRES_EXCEPTION_FPP
#define __FORTRES_EXCEPTION_FPP

#include "fortres/itfUtil.fpp"

#define _args_0
#define _args_1              arg1
#define _args_2     _args_1, arg2
#define _args_3     _args_2, arg3
#define _args_4     _args_3, arg4
#define _args_5     _args_4, arg5
#define _args_6     _args_5, arg6
#define _args_7     _args_6, arg7
#define _args_8     _args_7, arg8
#define _args_9     _args_8, arg9
#define _args_10    _args_9, arg10
#define _args_11    _args_10, arg11
#define _args_12    _args_11, arg12
#define _args_13    _args_12, arg13
#define _args_14    _args_13, arg14
#define _args_15    _args_14, arg15
#define _args_16    _args_15, arg16
#define _args_17    _args_16, arg17
#define _args_18    _args_17, arg18
#define _args_19    _args_18, arg19
#define _args_20    _args_19, arg20

# define _argEnd                 c_null_ptr
# define _argEnd_2    _argEnd,   c_null_ptr
# define _argEnd_3    _argEnd_2, c_null_ptr
# define _argEnd_4    _argEnd_3, c_null_ptr
# define _argEnd_5    _argEnd_4, c_null_ptr
# define _argEnd_6    _argEnd_5, c_null_ptr
# define _argEnd_7    _argEnd_6, c_null_ptr
# define _argEnd_8    _argEnd_7, c_null_ptr
# define _argEnd_9    _argEnd_8, c_null_ptr
# define _argEnd_10   _argEnd_9, c_null_ptr
# define _argEnd_11   _argEnd_10, c_null_ptr
# define _argEnd_12   _argEnd_11, c_null_ptr
# define _argEnd_13   _argEnd_12, c_null_ptr
# define _argEnd_14   _argEnd_13, c_null_ptr
# define _argEnd_15   _argEnd_14, c_null_ptr
# define _argEnd_16   _argEnd_15, c_null_ptr
# define _argEnd_17   _argEnd_16, c_null_ptr
# define _argEnd_18   _argEnd_17, c_null_ptr
# define _argEnd_19   _argEnd_18, c_null_ptr
# define _argEnd_20   _argEnd_19, c_null_ptr


#define _catch_1(a)                [a, 0]
#define _catch_2(a,b)              [a,b, 0]
#define _catch_3(a,b,c)            [a,b,c, 0]
#define _catch_4(a,b,c,d)          [a,b,c,d, 0]
#define _catch_5(a,b,c,d,e)        [a,b,c,d,e, 0]
#define _catch_6(a,b,c,d,e,f)      [a,b,c,d,e,f, 0]
#define _catch_7(a,b,c,d,e,f,g)    [a,b,c,d,e,f,g, 0]
#define _catch_8(a,b,c,d,e,f,g,h)  [a,b,c,d,e,f,g,h, 0]

#define _catch                     _catch_1
#define _catchAny                  [0]


!--------------------------------------------------------------------
! The block variants of try/catch can be generated by
!   the following macros.
! For all of them, there are some restrictions:
!  - only in subroutines
!  - containing subroutines must be declared recursive
!  - try blocks CAN NOT share local variables!
!  - try blocks CAN NOT appear in control structures
!      like IF, SELECT, DO, FORALL ...
!      => That's why there are "loop"-variants _tryDo and _tryFor
!  - take care of the given label numbers
!  - don't mix block types while declaring top and bottom of a block!
!
! Examples:
!   character(len=256) :: what
!   _tryBlock(10)
!     value = mightFail( x, y )
!   _tryCatch(10, _catchAny, what)
!   _tryEnd(10)
!
!   _tryDo(20)
!     value = mightFail( x, y )
!   _tryCatch(20, (ArithmeticError, RuntimeError), what)
!     case (ArithmeticError); continue
!     case (RuntimeError);    print *, "catched RuntimeError"
!                             _exitLoop(20)
!   _tryWhile(20, value < 0)
!
!   _tryFor(30, i = 0, i < 10, i = i + 1)
!     value = mightFail( x, i )
!   _tryCatch(20, (ArithmeticError, RuntimeError), what)
!     ! just ignore errors ...
!   _tryEndFor(30)
!--------------------------------------------------------------------

!-- start a simple try block
# define _tryBlock(label)         \
    goto _paste(label,02)        ;\
    entry _paste(tryblock__,label)

!-- start a try loop block (bottom-controlled: executes at least once)
# define _tryDo(label)  \
    _tryBlock(label)

!-- start a try loop block (top-controlled)
# define _tryFor(label, init, cond, inc ) \
    init                                 ;\
    goto _paste(label,01)                ;\
    _paste(label,00) inc                 ;\
    _paste(label,01) continue            ;\
    if (cond) goto _paste(label,02)      ;\
    goto _paste(label,03)                ;\
    entry _paste(tryblock__,label)


!-- start catch block, catching all given exception types --
# define _tryCatch(label, catchList, what)  \
    return                                 ;\
    _paste(label,02) continue              ;\
    select case( try( _catch(catchList), what, _paste(tryblock__,label) ) ) ;\
      case (0); continue  !< this is important! Without it gfortran would skip the try!


!-- end a simple try-catch
# define _tryEnd(label) \
    end select

!-- end a try-do
# define _tryWhile(label,cond)       \
    _tryEnd(label)                  ;\
    if (cond) goto _paste(label,02) ;\
    _paste(label,03) continue

!-- end a try-for
# define _tryEndFor(label) \
    end select            ;\
    goto _paste(label,00) ;\
    _paste(label,03) continue

!-- early exit 
!   RESTRICTIONS:
!    -> breaks only the most inner try-loop
!    -> works only within catch block
# define _exitLoop(label)  \
    goto _paste(label,03)

#endif 

