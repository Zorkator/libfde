#ifndef __FORTRES_TRACESTACK_HPP
#define __FORTRES_TRACESTACK_HPP

#include "fortres/StringRef.hpp"
#include "fortres/portdef.h"

typedef void (*FrameInfoOp)( StringRef *info );
typedef void (*TraceProcedure)( FrameInfoOp infoOp, const int *skippedFrames, StringRef *info );

_dllExport_C
void f_set_frameInfoOp( FrameInfoOp op );

_dllExport_C
void f_tracestack( FrameInfoOp infoOp, const int *skippedFrames, StringRef *info );

#endif /* __FORTRES_TRACESTACK_HPP */

