
#include <vector>
#include <set>
#include <csetjmp>
#include <string.h>
#include <stdarg.h>

//
// XXX: Portability issues
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
  typedef CodeSet::iterator                  CodeSetItr;


    CheckPoint( int *codeList, size_t len )
    : codes( codeList, codeList + len )
      { memset( &this->env, 0, sizeof(std::jmp_buf) ); }

  void
    handle( int code ) //< NOTE: might not return!
    {
      CodeSetItr it = this->codes.begin();

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
typedef void *                  ArgRef;
typedef std::vector<CheckPoint> CatchStack;

static CatchStack _catchStack;
static ArgRef     _argList[100];


extern "C" _dllExport
int _callingConv f_try( int *catchList, FortranProcedure proc, ... )
{
  va_list  vaArgs;
  size_t   nrArgs;

  va_start( vaArgs, proc );
  {
    // unpack list of given procedure arguments into _argList
    ArgRef *argPtr = _argList;
    while( *argPtr++ = va_arg( vaArgs, ArgRef ));
    nrArgs = (argPtr - _argList) - 1;
  }
  va_end( vaArgs );

  // convert 0-terminated catchList into CheckPoint and push it onto _catchStack
  int len;
  for (len = 0; catchList[len]; ++len);
  _catchStack.push_back( CheckPoint( catchList, len ) );

  // mark current stack location as point of return ...
  CheckPoint &here = _catchStack.back();
  int        code  = setjmp( here.env ); //< for marking setjmp returns 0!
  
  //<<< longjmp ends up here with some code different from 0!
  if (code == 0)
  {
    switch (nrArgs)
    {
      case 0: proc(); break;
      case 1: proc( _argList[0] ); break;
      case 2: proc( _argList[0], _argList[1] ); break;
      case 3: proc( _argList[0], _argList[1], _argList[2] ); break;
      case 4: proc( _argList[0], _argList[1], _argList[2], _argList[3] ); break;
      case 5: proc( _argList[0], _argList[1], _argList[2], _argList[3], _argList[4] ); break;
      default: throw; //<< can't handle number of given arguments!
    }
  }
  _catchStack.pop_back();
  return code;
}


#if 0 /* basic version - able to f_try only one specific procedure type */

typedef void (_callingConv *Func)( const char *argString, size_t len );

extern "C" _dllExport
int _callingConv f_try( Func func, int *catchList, int len )
{
  _catchStack.push_back( CheckPoint( catchList, len ) );

  CheckPoint &here = _catchStack.back();
  int        code  = setjmp( here.env );
  
  //< longjmp ends up here with code set different from 0!
  if (code == 0)
    { func( "testinger", 9 ); }
  _catchStack.pop_back();
  return code;
}
#endif


extern "C" _dllExport
void _callingConv f_throw( int code )
{
  while (_catchStack.size())
  {
    _catchStack.back().handle( code );
    _catchStack.pop_back();
  }
  /**
   * If we arrive here there's no matching catch point!
   * Hence, we just can't catch this exception ... sorry!
   */
  throw;
}

