#include <config.h>

#if defined HAVE_DBGHELP_H
# define _CRT_SECURE_NO_WARNINGS
# include <windows.h>
# include <DbgHelp.h>
#else
# include <execinfo.h>
#endif


#if defined HAVE_LIBLOADERAPI_H
# include <windows.h>
# include <libloaderapi.h>
#elif defined HAVE_DLFCN_H
# include <dlfcn.h>
#else
  #error "Neither HAVE_LIBLOADERAPI_H nor HAVE_DLFCN_H"
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/tracestack.hpp"
#include "fortres/dirent.hpp"


void
f_printFrameLine( StringRef *frameInfo )
  { fprintf( stderr, "%.*s\n", static_cast<int>(frameInfo->length()), frameInfo->buffer() ); }

/**
 * Allow user specified frameLine-function, which gets called
 *  for all StandardError exception types.
 */
static FrameInfoOp _frameInfoOp = f_printFrameLine;

_dllExport_C
void
f_set_frameInfoOp( FrameInfoOp op )
  { _frameInfoOp = op; }


_dllExport_C
void
f_tracestack( FrameInfoOp infoOp, const int *skippedFrames, StringRef *info )
{
  void     *frames[100];
  int       stackSize;
  StringRef infoRef;

  /* make sure there is a FrameInfoOp */
  if (infoOp == NULL) infoOp = _frameInfoOp;
  if (infoOp == NULL) infoOp = f_printFrameLine;

#if defined HAVE_DBGHELP_H
  HANDLE       process;
  char         frameBuffer[512+32];
  char         symbolBuffer[sizeof(SYMBOL_INFO) + 256 * sizeof(char)];
  SYMBOL_INFO *symbol = (SYMBOL_INFO*)symbolBuffer;

  process   = GetCurrentProcess();
  SymInitialize( process, NULL, TRUE );
  stackSize = CaptureStackBackTrace( 0, sizeof(frames), frames, NULL );

  symbol->MaxNameLen   = 255;
  symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

  for (int i = stackSize - 1; i > *skippedFrames; --i)
  {
    SymFromAddr( process, (DWORD64)(frames[i]), 0, symbol );
    so_filepath_of( (void *)symbol->Address, frameBuffer, 256 );
    sprintf( frameBuffer, "%s: %s [0x%0X]", frameBuffer, symbol->Name, symbol->Address );
    infoOp( &infoRef.referTo(frameBuffer).trim() );
  }
#else
  char **strings;

  stackSize = backtrace( frames, sizeof(frames) );
  strings   = backtrace_symbols( frames, stackSize );

  if (strings)
  {
    for (int i = stackSize - 1; i > *skippedFrames; --i)
      { infoOp( &infoRef.referTo(strings[i]).trim() ); }
      free( strings );
  }
#endif
  
  if (info && info->length())
    { infoOp( info ); }
}

