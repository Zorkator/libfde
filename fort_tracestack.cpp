
#if defined _MSC_VER
# include <windows.h>
# include <DbgHelp.h>
#else
# include <dlfcn.h>
# include <execinfo.h>
#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define  _DLL_EXPORT_IMPLEMENTATION_
#include "fortres/tracestack.hpp"
#include "fortres/sharedlib.hpp"
#include "fortres/dirent.hpp"


_dllExport_C
void
f_tracestack( FrameOperator frameOp, int skippedFrames )
{
  void     *frames[100];
  int       stackSize;
	StringRef infoRef;

#if defined _MSC_VER
  HANDLE       process;
  SYMBOL_INFO *symbol;
  char         frameBuffer[512+32];

  process   = GetCurrentProcess();
  SymInitialize( process, NULL, TRUE );
  stackSize = CaptureStackBackTrace( 0, sizeof(frames), frames, NULL );

  symbol = (SYMBOL_INFO *)malloc( sizeof(SYMBOL_INFO) + 256 * sizeof(char) );
  symbol->MaxNameLen   = 255;
  symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

  for (int i = stackSize - 1; i > skippedFrames; --i)
  {
    SymFromAddr( process, (DWORD64)(frames[i]), 0, symbol );
    so_filepath_of( (void *)symbol->Address, frameBuffer, 256 );
    sprintf( frameBuffer, "%s(%s) [0x%0X]", frameBuffer, symbol->Name, symbol->Address );
    frameOp( &infoRef.referTo(frameBuffer) );
  }
  free( symbol );
#else
	char **strings;

	stackSize = backtrace( frames, sizeof(frames) );
	strings   = backtrace_symbols( frames, stackSize );

	if (strings)
	{
    for (int i = stackSize - 1; i > skippedFrames; --i)
			{ frameOp( &infoRef.referTo(strings[i]) ); }
		free( strings );
	}
#endif
}

