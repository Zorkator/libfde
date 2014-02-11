
f90_include_dirs    = .
f90_pp              = full
f90_pp_flags        = BUILT_TYPE=SHARED_LIB
f90_options         = pic shared
f90_options_debug   = debug
f90_options_release = O3
f90_files           = string_ref.f90 exception.f90 main.f90
f90_moduleLib       =
f90_library_dirs    = .
f90_link_options    =
f90_link_libs       = fortres
f90_link_output     = fortresTest

user_configurations = special

# generic: rules & settings
#
mk_configurations = debug release $(user_configurations: % =%)
mk_config         = $(if $(_config),$(_config),debug)

# f90: rules & settings ...
#
# compiler specifics ...
mk_f90_compiler    = ifort
mk_f90_includePath = -I
mk_f90_modulePath  = -module
mk_f90_ppFlag      = -D
mk_f90_compile     = -c
mk_f90_out         = -o
mk_f90_pp_off      =
mk_f90_pp_on       = -fpp
mk_f90_pp_full     = -fpp -allow nofpp-comments
mk_f90_pp_debug    = $(mk_f90_pp_full) -save-temps
mk_f90_pic         = -fpic
mk_f90_shared      = -shared
mk_f90_debug       = -debug full
mk_f90_O0          = -O0
mk_f90_O1          = -O1
mk_f90_O2          = -O2
mk_f90_O3          = -O3
mk_f90_libraryPath = -L
mk_f90_linkLib     = -l
mk_f90_tmps        = *.s *.i90

# mk_f90_compiler    = gfortan
# mk_f90_includePath = -I
# mk_f90_modulePath  = -J
# mk_f90_ppFlag      = -D
# mk_f90_compile     = -c
# mk_f90_out         = -o
# mk_f90_pp_off      =
# mk_f90_pp_on       = -x f95-cpp-input
# mk_f90_pp_full     = $(mk_f90_pp_on) -ffree-line-length-none

# derived ...
mk_f90_I_dirs       = $(f90_include_dirs:%=$(mk_f90_includePath)%) $(mk_f90_includePath)./$(mk_config)
mk_f90_module_dirs  = $(mk_f90_modulePath) ./$(mk_config)
mk_f90_fpp_cmd      = $(mk_f90_pp_$(f90_pp))
mk_f90_fpp_flags    = $(f90_pp_flags:%=$(mk_f90_ppFlag)%)
mk_f90_fpp          = $(mk_f90_fpp_cmd) $(mk_f90_fpp_flags)
mk_f90_opts         = $(f90_options) $(f90_options_$(mk_config))
mk_f90_options      = $(foreach opt,$(mk_f90_opts:%=mk_f90_%),$($(opt)))
mk_f90_files        = $(wildcard $(f90_files))
mk_f90_objects      = $(mk_f90_files:%.f90=$(mk_config)/%.fo)
mk_f90_modules      = $(wildcard $(mk_config)/*.mod)
mk_f90_moduleLib    = $(mk_config)/$(if $(f90_moduleLib),$(f90_moduleLib),modules.lib)
mk_f90_L_dirs       = $(f90_library_dirs:%=$(mk_f90_libraryPath)%)
mk_f90_link_libs    = $(f90_link_libs:%=$(mk_f90_linkLib)%)
mk_f90_link_options = $(foreach opt,$(f90_link_options:%=mk_f90_%),$($(opt)))
mk_f90_link_output  = $(mk_config)/$(f90_link_output)
mk_f90_tmp_files    = $(wildcard $(mk_f90_tmps))

$(mk_config)/%.fo: %.f90
	$(mk_f90_compiler) $(mk_f90_fpp) $(mk_f90_options) $(mk_f90_I_dirs) $(mk_f90_module_dirs) $(mk_f90_compile) $< $(mk_f90_out) $@


# debug target, printing variable contents
#
echo_%:
	@echo "$* = $($*)"

.PHONY: all objects clean $(mk_configurations)

all: $(mk_config)\/ $(mk_f90_link_output)
	@echo "done"

$(mk_config)\/:
	mkdir -p $@

$(mk_configurations):
	mkdir -p $@
	$(MAKE) _config=$@ all

clean:
	rm -f $(mk_f90_objects) $(mk_f90_modules) $(mk_f90_moduleLib) $(mk_f90_tmp_files)

$(mk_f90_link_output): $(mk_f90_objects)
	$(mk_f90_compiler) $(mk_f90_objects) $(mk_f90_link_options) $(mk_f90_L_dirs) $(mk_f90_link_libs) $(mk_f90_out) $@
	
objects: $(mk_f90_objects)
	ar rc $(mk_f90_moduleLib) $(mk_config)/*.mod

