#ifndef __FORTRES_TRACESTACK_HPP
#define __FORTRES_TRACESTACK_HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

typedef void (*FrameOperator)( StringRef *frameInfo );

_dllExport_C
void f_tracestack( FrameOperator frameOp, int skippedFrames );

#endif /* __FORTRES_TRACESTACK_HPP */

