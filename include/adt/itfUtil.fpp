#ifndef __ADT_ITFUTIL_FPP
#define __ADT_ITFUTIL_FPP

#include "fortres/itfUtil.fpp"

!---------------------------------------
! ADT specific extensions go here ...
!---------------------------------------

#define _optArg(var, arg, default)       \
  if (present(arg)) then; var = arg     ;\
                    else; var = default ;\
  end if

#define _optArgPtr(ptr, arg, default)     \
  if (present(arg)) then; ptr => arg     ;\
                    else; ptr => default ;\
  end if


#endif

