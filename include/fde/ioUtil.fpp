#ifndef __FDE_IOUTIL_FPP
#define __FDE_IOUTIL_FPP


# ifndef CONSOLE_IO
!   enable console input/ouput by default
#   define CONSOLE_IO ENABLED
# endif


# if CONSOLE_IO
!   console io enabled ...
!   NOTE: this requires the definition of status variable IOS!
#   define CONSOLE_ERR(msg)               WRITE(0, '(2X,A)', iostat=IOS) adjustl(msg)
#   define CONSOLE_MSG(msg)               WRITE(6, '(2X,A)', iostat=IOS) adjustl(msg)
#   define CONSOLE_WRITE(fmt)             WRITE(6, fmt, iostat=IOS)
#   define CONSOLE_WAIT()                 CONSOLE_MSG('PRESS ENTER TO CONTINUE'); \
                                          READ (5, '(A)', iostat=IOS)

#   define _1arg(a)                       , a
#   define _2args(a,b)                    , a, b
#   define _3args(a,b,c)                  , a, b, c
#   define _4args(a,b,c,d)                , a, b, c, d
#   define _5args(a,b,c,d,e)              , a, b, c, d, e
#   define _6args(a,b,c,d,e,f)            , a, b, c, d, e, f
#   define _7args(a,b,c,d,e,f,g)          , a, b, c, d, e, f, g
#   define _8args(a,b,c,d,e,f,g,h)        , a, b, c, d, e, f, g, h
#   define _9args(a,b,c,d,e,f,g,h,i)      , a, b, c, d, e, f, g, h, i
#   define _10args(a,b,c,d,e,f,g,h,i,j)   , a, b, c, d, e, f, g, h, i, j

#   define _dump(a)                       , " " // #a // "=", a

# else
!   console io disabled ...
#   define CONSOLE_ERR(msg)               CONTINUE
#   define CONSOLE_MSG(msg)               CONTINUE
#   define CONSOLE_WRITE(fmt)             CONTINUE
#   define CONSOLE_WAIT()                 CONTINUE

#   define _1arg(a)
#   define _2args(a,b)
#   define _3args(a,b,c)
#   define _4args(a,b,c,d)
#   define _5args(a,b,c,d,e)
#   define _6args(a,b,c,d,e,f)
#   define _7args(a,b,c,d,e,f,g)
#   define _8args(a,b,c,d,e,f,g,h)
#   define _9args(a,b,c,d,e,f,g,h,i)
#   define _10args(a,b,c,d,e,f,g,h,i,j)

#   define _dump(a)

#endif


# ifdef _WIN32
#   define env_HOSTNAME   "COMPUTERNAME"
#   define _slash         "\"
# else
#   define env_HOSTNAME   "HOSTNAME"
#   define _slash         "/"
# endif

#endif

