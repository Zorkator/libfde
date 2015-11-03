
#include <csetjmp>
#include <stdarg.h>
#include <algorithm>
#include <functional>
#include <vector>
#include <map>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/exception.hpp"

class ExceptionMap
: public std::map<int, std::string>
{
  public:
      typedef std::map<int, std::string>  Type;
      typedef Type::iterator              Iterator;

      ExceptionMap( void )
      {
#       define _fortres_exception_type(_ident, _num) \
          (*this)[_paste(0x,_num)] = _str(_ident);
        _fortres_ExceptionTable
        (*this)[0] = "_invalid_Exception";
#       undef  _fortres_exception_type
      }

    const std::string &
      get( int code )
      {
        Iterator itr = this->find( code );
        if (itr != this->end())
          { return itr->second; }
        else
          { return (*this)[0]; }
      }
};

static ExceptionMap exceptionMap;

class Context
{
  private:
    struct CheckPoint
    {
      typedef std::vector<int>        CodeSet;
      typedef std::vector<Procedure>  ProcList;

      
        CheckPoint( int *codeList, size_t len )
        : _codes( codeList, codeList + len )
        {
          memset( &_env, 0, sizeof(std::jmp_buf) );
          std::sort( _codes.begin(), _codes.end(), std::greater<int>() );
        }


      void
        check( int code, const StringRef *what ) //< NOTE: might not return!
        {
          CodeSet::iterator  it            = _codes.begin();
          const std::string &exceptionName = exceptionMap.get( code );

          // iterate over codes (sorted by descending order!)
          // This means we get the exception codes from most to least specific.
          for (; it != _codes.end(); ++it)
          {
            if ((*it & code) == *it)
            {
              code = *it; //< use most specific exception code found
              break;
            }
          }

          // call cleanup procedures in reverse order ...
          for (size_t i = _procs.size(); i > 0; --i)
            { _procs[i-1](); }

          if (_codes.empty() || it != _codes.end())
          {
            _msg = exceptionName + ": ";
            _msg.append( what->str() );
            std::longjmp( _env, code );
          }
        }

      void
        push( Procedure proc )
          { _procs.push_back( proc ); }

      void
        pop( bool exec )
        {
          if (exec)
            { _procs.back()(); }
          _procs.pop_back();
        }
        
      // members ...
      std::jmp_buf  _env;
      CodeSet       _codes;
      ProcList      _procs;
      std::string   _msg;
    };

    std::vector<CheckPoint> _checkPoints;

  public:
    std::jmp_buf &
      openFrame( int *codeList ) //< expects 0-terminated codeList!
      {
        size_t len;
        for (len = 0; codeList[len]; ++len);
        _checkPoints.push_back( CheckPoint( codeList, len ) );
        return _checkPoints.back()._env;
      }
    
    void
      closeFrame( void )
        { _checkPoints.pop_back(); }

    void
      handle( int code, const StringRef *what )
      {
        while (_checkPoints.size())
        {
          _checkPoints.back().check( code, what );
          _checkPoints.pop_back();
        }
      }

    void
      push_cleanup( Procedure proc )
        { _checkPoints.back().push( proc ); }

    void
      pop_cleanup( bool exec )
        { _checkPoints.back().pop( exec ); }

    std::string &
      message( void )
        { return _checkPoints.back()._msg; }
};


/**
 * The following routines try to get the try-catch mechanism suitable for threading.
 * Since there are various types of threading, there is no "standard way" to synchronize threads
 *   and to keep the CatchStacks separated.
 * Additionally, we don't know what type of threading is applied by the program using libfortres.
 * Thats why we handle this as "not-my-business" and expect the program to hook a synchronizer procedure.
 * Basically, such procedure could be kept as easy as the following example (C-pseudo):
 * 
 * void my_synchronizer( Context **context, int )
 * {
 *   mutex->lock();
 *     f_get_context( context, get_current_thread_id() );
 *   mutex->release();
 * }
 */

_dllExport_C
void
f_get_context( Context **context, int contextId )
{
  static std::map<int, Context>  _contextMap;
  *context = &_contextMap[contextId];
}

Synchronizer _synchronizer = f_get_context;


_dllExport_C
void
f_set_synchronizer( Synchronizer proc )
  { _synchronizer = proc; }


inline Context *
getContext( void )
{
  Context *context = NULL;
  _synchronizer( &context, 0 );
  return context;
}


/**
 * Allow user specified trace-function, which gets called by throw
 *  for all StandardError exception types.
 */
Procedure _traceproc = NULL;

_dllExport_C
void
f_set_traceproc( Procedure proc )
  { _traceproc = proc; }


/**
 * The remaining routines care for marking catch points (f_try), throwing exceptions (f_throw)
 *   and registering code that needs to be executed exception-safe (e.g. cleanup-code).
 */

_dllExport_C
int
f_try( int *catchList, StringRef *what, Procedure proc, ... )
{
  typedef  void *  ArgRef;

  va_list  vaArgs;
  ArgRef   argBuf[32];
  size_t   nrArgs;
  Context *context;

  va_start( vaArgs, proc );
  {
    // unpack list of given procedure arguments into argBuf
    ArgRef *argPtr = argBuf;
    while( *argPtr++ = va_arg( vaArgs, ArgRef ));
    nrArgs = (argPtr - argBuf) - 1;
  }
  va_end( vaArgs );

  context  = getContext();
  int code = setjmp( context->openFrame( catchList ) ); //< mark current stack location as point of return ...
                                                        //  NOTE that setjmp returns 0 for marking!
  
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
    what->erase();
  }
  else
  {
    // Here, we've just landed the longjmp ...
    *what = context->message();
  }

  context->closeFrame();
  return code;
}


_dllExport_C
void
f_throw( int code, const StringRef *what )
{
  if (_traceproc && (code & StandardError))
    { _traceproc(); }

  getContext()->handle( code, what );
  /**
   * If we arrive here there's no matching catch point!
   * This means, we just can't catch this exception ... sorry and bye!
   */
  throw;
}


_dllExport_C
void
f_push_cleanup( Procedure proc )
{
  getContext()->push_cleanup( proc );
}


_dllExport_C
void
f_pop_cleanup( int exec )
{
  getContext()->pop_cleanup( exec != 0 );
}

