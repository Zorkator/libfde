#include <fortres_config.h>

#if defined HAVE_DBGHELP_H
# define NOMINMAX
# define _CRT_SECURE_NO_WARNINGS
# include <windows.h>
# include <DbgHelp.h>
#else
# include <execinfo.h>
#endif


#if defined HAVE_LIBLOADERAPI_H
# define NOMINMAX
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
#include <algorithm>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/tracestack.hpp"
#include "fortres/dirent.hpp"

#define arraySize(a)    (sizeof(a)/sizeof(a[0]))

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
  stackSize = CaptureStackBackTrace( 0, arraySize(frames), frames, NULL );

  symbol->MaxNameLen   = 255;
  symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

  //                             . o O (we skip frame[0] that refers to most inner frame f_tracestac)
  for (int i = stackSize - 1; i > std::max( 0, *skippedFrames ); --i)
  {
    SymFromAddr( process, (DWORD64)(frames[i]), 0, symbol );
    so_filepath_of( (void *)symbol->Address, frameBuffer, 256 );
    sprintf( frameBuffer, "%s: %s [0x%0llX]", frameBuffer, symbol->Name, (ULONG64)symbol->Address );
    infoOp( &infoRef.referTo(frameBuffer).trim() );
  }
#else
  char **strings;

  stackSize = backtrace( frames, arraySize(frames) );
  strings   = backtrace_symbols( frames, stackSize );

  if (strings)
  {
    //                             . o O (we skip strings[0] that refers to most inner frame f_tracestac)
    for (int i = stackSize - 1; i > std::max( 0, *skippedFrames ); --i)
      { infoOp( &infoRef.referTo(strings[i]).trim() ); }
      free( strings );
  }
#endif

  if (info && info->length())
    { infoOp( info ); }
}

