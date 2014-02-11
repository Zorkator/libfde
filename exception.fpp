#ifndef __EXCEPTION_FPP
#define __EXCEPTION_FPP

#define _0_args()   
#define _1_arg()    arg1,
#define _2_args()   arg1, arg2,
#define _3_args()   arg1, arg2, arg3,
#define _4_args()   arg1, arg2, arg3, arg4,
#define _5_args()   arg1, arg2, arg3, arg4, arg5,


#define _tryProcedure( id, args ) \
   integer*4 function id( catchList, sub, args() theEnd ) bind(C,name="f_try") ;\
     use, intrinsic :: iso_c_binding; use arg_terminator                         


#define _end_tryProcedure \
     integer(kind=c_int), intent(in)   :: catchList(*)                           ;\
     procedure ()                      :: sub                                    ;\
     type (NeverProvideThis), optional :: theEnd                                 ;\
   end function


#define _catch(cases)   [cases, 0]
#define _catchAny       [0]

#endif 

