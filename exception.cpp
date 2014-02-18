
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <csetjmp>
#include <string.h>
#include <stdarg.h>

//
// TODO: Portability issues
//  * replace __cdecl calling convention by some macro definition

//# define _callingConv   __cdecl
# define _callingConv
#if defined _WIN32
#	define _dllExport		__declspec(dllexport)
#else
#	define _dllExport
#endif

struct CheckPoint
{
  // Note: for storing the exception codes we use a ordered set, with descending order!
  typedef std::set<int, std::greater<int> >  CodeSet;


    CheckPoint( int *codeList, size_t len )
    : codes( codeList, codeList + len )
      { memset( &this->env, 0, sizeof(std::jmp_buf) ); }


  void
    handle( int code ) //< NOTE: might not return!
    {
      CodeSet::iterator it = this->codes.begin();

      // iterate over codes in descending order
      // This means we get the exception codes from most to least specific.
      for (; it != this->codes.end(); ++it)
      {
        if ((*it & code) == *it)
          { break; }
      }

      if (this->codes.empty() || it != this->codes.end())
        { std::longjmp( this->env, *it ); }
    }

  std::jmp_buf  env;
  CodeSet       codes;
};

typedef void (*FortranProcedure)( ... );
typedef void *                      ArgRef;
typedef std::vector<CheckPoint>     CatchStack;
typedef std::map<int, CatchStack>   ContextMap;


/**
 * Note, that this try-catch mechanism NOT fit for threading yet!
 * This is mostly because of one static CatchStack.
 * Even synchronized, it would happily mix up CheckPoints that originate from try-calls
 *   of different threads - and that's surely not healthy!
 * We'd rather have to associate a separate CatchStack to each thread.
 * This could be fixed easily by mapping the thread-id to a CatchStack.
 * However, since there's no theadding standard yet (thanks M$!) this causes again
 *   portability issues.
 */

inline CatchStack &
getContext( void )
{
  static ContextMap _contextMap;

  // TODO: synchronize here!
  int threadId = 0 /*<< TODO: replace this by current thread-Id! */;
  return _contextMap[threadId];
}


extern "C" _dllExport
int
_callingConv f_try( int *catchList, FortranProcedure proc, ... )
{
  va_list  vaArgs;
  ArgRef   argBuf[32];
  size_t   nrArgs;

  va_start( vaArgs, proc );
  {
    // unpack list of given procedure arguments into argBuf
    ArgRef *argPtr = argBuf;
    while( *argPtr++ = va_arg( vaArgs, ArgRef ));
    nrArgs = (argPtr - argBuf) - 1;
  }
  va_end( vaArgs );

  // convert 0-terminated catchList into CheckPoint and push it onto catchStack
  int len;
  for (len = 0; catchList[len]; ++len);

  CatchStack &catchStack = getContext();
  catchStack.push_back( CheckPoint( catchList, len ) );

  // mark current stack location as point of return ...
  CheckPoint &here = catchStack.back();
  int        code  = setjmp( here.env ); //< for marking setjmp returns 0!
  
  //<<< longjmp ends up here with some code different from 0!
  if (code == 0)
  {
    switch (nrArgs)
    {
#     define expandArgs_0     
#     define expandArgs_1     argBuf[0]
#     define expandArgs_2     expandArgs_1, argBuf[1]
#     define expandArgs_3     expandArgs_2, argBuf[2]
#     define expandArgs_4     expandArgs_3, argBuf[3]
#     define expandArgs_5     expandArgs_4, argBuf[4]
#     define expandArgs_6     expandArgs_5, argBuf[5]
#     define expandArgs_7     expandArgs_6, argBuf[6]
#     define expandArgs_8     expandArgs_7, argBuf[7]
#     define expandArgs_9     expandArgs_8, argBuf[8]
#     define expandArgs_10    expandArgs_9, argBuf[9]
#     define expandArgs_11    expandArgs_10, argBuf[10]
#     define expandArgs_12    expandArgs_11, argBuf[11]
#     define expandArgs_13    expandArgs_12, argBuf[12]
#     define expandArgs_14    expandArgs_13, argBuf[13]
#     define expandArgs_15    expandArgs_14, argBuf[14]
#     define expandArgs_16    expandArgs_15, argBuf[15]
#     define expandArgs_17    expandArgs_16, argBuf[16]
#     define expandArgs_18    expandArgs_17, argBuf[17]
#     define expandArgs_19    expandArgs_18, argBuf[18]
#     define expandArgs_20    expandArgs_19, argBuf[19]
#     define expandArgs(nr)   expandArgs_ ## nr

      case  0: proc( expandArgs( 0) ); break;
      case  1: proc( expandArgs( 1) ); break;
      case  2: proc( expandArgs( 2) ); break;
      case  3: proc( expandArgs( 3) ); break;
      case  4: proc( expandArgs( 4) ); break;
      case  5: proc( expandArgs( 5) ); break;
      case  6: proc( expandArgs( 6) ); break;
      case  7: proc( expandArgs( 7) ); break;
      case  8: proc( expandArgs( 8) ); break;
      case  9: proc( expandArgs( 9) ); break;
      case 10: proc( expandArgs(10) ); break;
      case 11: proc( expandArgs(11) ); break;
      case 12: proc( expandArgs(12) ); break;
      case 13: proc( expandArgs(13) ); break;
      case 14: proc( expandArgs(14) ); break;
      case 15: proc( expandArgs(15) ); break;
      case 16: proc( expandArgs(16) ); break;
      case 17: proc( expandArgs(17) ); break;
      case 18: proc( expandArgs(18) ); break;
      case 19: proc( expandArgs(19) ); break;
      case 20: proc( expandArgs(20) ); break;
      default: throw; //<< can't handle number of given arguments!
    }
  }
  catchStack.pop_back();
  return code;
}


extern "C" _dllExport
void
_callingConv f_throw( int code )
{
  CatchStack &catchStack = getContext();

  while (catchStack.size())
  {
    catchStack.back().handle( code );
    catchStack.pop_back();
  }
  /**
   * If we arrive here there's no matching catch point!
   * Hence, we just can't catch this exception ... sorry!
   */
  throw;
}

