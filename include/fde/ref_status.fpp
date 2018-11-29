#ifndef __FDE_REF_STATUS_FPP
#define __FDE_REF_STATUS_FPP

# define _RefStatus \
    integer*1, dimension(2)

# define _ref_WeakLent        (/0, 0/)
# define _ref_WeakMine        (/0, 1/)
# define _ref_HardLent        (/1, 0/)
# define _ref_HardMine        (/1, 1/)

# define _ref_init(s,h)       s(1:2) = (/int(h,1), int(0,1)/)
# define _ref_initMine(s,h)   s(1:2) = (/int(h,1), int(1,1)/)

# define _ref_setHard(s,v)    s(1) = v
# define _ref_setMine(s,v)    s(2) = v

# define _ref_isHard(s)       s(1) > 0
# define _ref_isMine(s)       s(2) > 0
# define _ref_isWeakLent(s)   (s(1) == 0 .and. s(2) == 0)
# define _ref_isWeakMine(s)   (s(1) == 0 .and. s(2) >  0)
# define _ref_isHardLent(s)   (s(1) >  0 .and. s(2) == 0)
# define _ref_isHardMine(s)   (s(1) >  0 .and. s(2) >  0)

# define _ref_hardness(s)     s(1)
# define _ref_ownership(s)    s(2)

#endif

