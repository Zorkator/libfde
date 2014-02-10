#ifndef __EXCEPTION_BASE_FPP
#define __EXCEPTION_BASE_FPP

#define _0Args()   
#define _1Args()   , arg1
#define _2Args()   , arg1, arg2
#define _3Args()   , arg1, arg2, arg3
#define _4Args()   , arg1, arg2, arg3, arg4
#define _5Args()   , arg1, arg2, arg3, arg4, arg5

#define _TryITF( id, args ) \
   integer*4 function id( catchList, sub args(), theEnd ) &\
     bind(C,name="tryCall")                               ;\
     use, intrinsic :: iso_c_binding                      ;\
     import NeverProvideThis, StringRef                   ;\
     integer(kind=c_int), intent(in)   :: catchList(*)    ;\
     procedure ()                      :: sub             ;\
     type (NeverProvideThis), optional :: theEnd

#define _end_TryITF \
   end function

#endif 

