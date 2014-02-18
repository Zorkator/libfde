#ifndef __EXCEPTION_FPP
#define __EXCEPTION_FPP

#define _args_0()
#define _args_1()     arg1,
#define _args_2()     _args_1() arg2,
#define _args_3()     _args_2() arg3,
#define _args_4()     _args_3() arg4,
#define _args_5()     _args_4() arg5,
#define _args_6()     _args_5() arg6,
#define _args_7()     _args_6() arg7,
#define _args_8()     _args_7() arg8,
#define _args_9()     _args_8() arg9,
#define _args_10()    _args_9() arg10,
#define _args_11()    _args_10() arg11,
#define _args_12()    _args_11() arg12,
#define _args_13()    _args_12() arg13,
#define _args_14()    _args_13() arg14,
#define _args_15()    _args_14() arg15,
#define _args_16()    _args_15() arg16,
#define _args_17()    _args_16() arg17,
#define _args_18()    _args_17() arg18,
#define _args_19()    _args_18() arg19,
#define _args_20()    _args_19() arg20,
#define _args(nr)     _args_ ## nr()


#define _tryProcedure( id, args ) \
   function id( catchList, sub, args() argEnd ) bind(C,name="f_try") result(res) ;\
     use, intrinsic :: iso_c_binding


#define _end_tryProcedure \
     integer(kind=c_int), intent(in)    :: catchList(*)  ;\
     type (c_funptr), value, intent(in) :: sub           ;\
     type (c_ptr),    value, intent(in) :: argEnd        ;\
     integer(kind=c_int)                :: res           ;\
   end function


#define _catch(cases)   [cases, 0]
#define _catchAny       [0]
#define _argEnd         c_null_ptr

#endif 

