#if defined _MSC_VER
#define HAVE_WINDOWS_H 1
#define HAVE_STRING_SECURE 1
/* #undef HAVE_SETENV */
#define HAVE_DBGHELP_H 1
#define HAVE_LIBLOADERAPI_H 1
#define HAVE_PSAPI_H 1

/* #undef HAVE_DIRENT_H */
/* #undef HAVE_DLFCN_H */
/* #undef HAVE_LINK_H */


#define LIB_PREFIX        ""
#define LIB_SUFFIX        ".dll"
#define LIB_PATH_VAR      "PATH"
#define LIB_PATH_SEP      ";"
#define PATH_SEP          "\\"
#define PATH_SEP_UNIX     "/"
#define PATH_SEP_WINDOWS  "\\"



#else

/* #undef HAVE_WINDOWS_H */
/* #undef HAVE_STRING_SECURE */
#define HAVE_SETENV 1
/* #undef HAVE_DBGHELP_H */
/* #undef HAVE_LIBLOADERAPI_H */
/* #undef HAVE_PSAPI_H */

#define HAVE_DIRENT_H 1
#define HAVE_DLFCN_H 1
#define HAVE_LINK_H 1


#define LIB_PREFIX        "lib"
#define LIB_SUFFIX        ".so"
#define LIB_PATH_VAR      "LD_LIBRARY_PATH"
#define LIB_PATH_SEP      ":"
#define PATH_SEP          "/"
#define PATH_SEP_UNIX     "/"
#define PATH_SEP_WINDOWS  "\\"


#endif
