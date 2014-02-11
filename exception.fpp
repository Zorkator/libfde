#ifndef __EXCEPTION_FPP
#define __EXCEPTION_FPP

#define _0_args()   
#define _1_arg()    arg1,
#define _2_args()   arg1, arg2,
#define _3_args()   arg1, arg2, arg3,
#define _4_args()   arg1, arg2, arg3, arg4,
#define _5_args()   arg1, arg2, arg3, arg4, arg5,


#define _tryProcedure( id, args ) \
   integer(kind=c_int) function id( catchList, sub, args() argEnd ) bind(C,name="f_try") ;\
     use, intrinsic :: iso_c_binding


#define _end_tryProcedure \
     integer(kind=c_int), intent(in)    :: catchList(*)  ;\
     type (c_funptr), value, intent(in) :: sub           ;\
     type (c_ptr),    value, intent(in) :: argEnd        ;\
   end function


#define _catch(cases)   [cases, 0]
#define _catchAny       [0]
#define _argEnd         c_null_ptr

#endif 

