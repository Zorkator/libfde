
OUT_TYPE        = exe
OUT_NAME        = example
OUT_DIR         = bin
FC_INCLUDE_DIRS = ../include
FC_CFLAGS.%     = $(fc_fpp) $(call fc_form,free,none) $(fc_backtrace)
FC_LIBRARY_DIRS = ../release
FC_LIBRARIES    = fortres.x$(mk_ARCH)

ifneq ($(MAKEIT_DIR),)
include $(MAKEIT_DIR)/mk.fortran
else
$(error environment variable MAKEIT_DIR not set! Please set it to home directory of makeIt)
endif
