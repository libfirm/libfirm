cmake_minimum_required(VERSION 3.0)
project(libfirm VERSION 1.22.0)
set(PROJECT_DESCRIPTION "library implementing the intermediate representation Firm")

set(CMAKE_C_VISIBILITY_PRESET hidden)

set(SOURCES
	ir/adt/array.c
	ir/adt/bipartite.c
	ir/adt/bitset.c
	ir/adt/cpset.c
	ir/adt/gaussjordan.c
	ir/adt/gaussseidel.c
	ir/adt/hungarian.c
	ir/adt/pdeq.c
	ir/adt/plist.c
	ir/adt/pmap.c
	ir/adt/pqueue.c
	ir/adt/pset.c
	ir/adt/pset_new.c
	ir/adt/set.c
	ir/adt/xmalloc.c
	ir/ana/analyze_irg_args.c
	ir/ana/callgraph.c
	ir/ana/cdep.c
	ir/ana/cgana.c
	ir/ana/constbits.c
	ir/ana/dca.c
	ir/ana/dfs.c
	ir/ana/domfront.c
	ir/ana/execfreq.c
	ir/ana/heights.c
	ir/ana/irbackedge.c
	ir/ana/ircfscc.c
	ir/ana/irconsconfirm.c
	ir/ana/irdom.c
	ir/ana/irlivechk.c
	ir/ana/irloop.c
	ir/ana/irmemory.c
	ir/ana/irouts.c
	ir/ana/vrp.c
	ir/be/amd64/amd64_cconv.c
	ir/be/amd64/amd64_emitter.c
	ir/be/amd64/amd64_finish.c
	ir/be/amd64/amd64_new_nodes.c
	ir/be/amd64/amd64_pic.c
	ir/be/amd64/amd64_transform.c
	ir/be/amd64/amd64_varargs.c
	ir/be/amd64/bearch_amd64.c
	ir/be/arm/arm_cconv.c
	ir/be/arm/arm_emitter.c
	ir/be/arm/arm_finish.c
	ir/be/arm/arm_lower64.c
	ir/be/arm/arm_new_nodes.c
	ir/be/arm/arm_optimize.c
	ir/be/arm/arm_transform.c
	ir/be/arm/bearch_arm.c
	ir/be/bearch.c
	ir/be/beasm.c
	ir/be/beblocksched.c
	ir/be/bechordal.c
	ir/be/bechordal_common.c
	ir/be/bechordal_main.c
	ir/be/becopyheur4.c
	ir/be/becopyilp.c
	ir/be/becopyilp2.c
	ir/be/becopyopt.c
	ir/be/bediagnostic.c
	ir/be/bedump.c
	ir/be/bedwarf.c
	ir/be/beemitter.c
	ir/be/beemitter_binary.c
	ir/be/beflags.c
	ir/be/begnuas.c
	ir/be/beifg.c
	ir/be/beinfo.c
	ir/be/beinsn.c
	ir/be/beirg.c
	ir/be/belistsched.c
	ir/be/belive.c
	ir/be/beloopana.c
	ir/be/belower.c
	ir/be/bemain.c
	ir/be/bemodule.c
	ir/be/benode.c
	ir/be/bepbqpcoloring.c
	ir/be/bepeephole.c
	ir/be/beprefalloc.c
	ir/be/bera.c
	ir/be/besched.c
	ir/be/beschednormal.c
	ir/be/beschedrand.c
	ir/be/beschedtrivial.c
	ir/be/bespill.c
	ir/be/bespillbelady.c
	ir/be/bespilldaemel.c
	ir/be/bespillslots.c
	ir/be/bespillutil.c
	ir/be/bessaconstr.c
	ir/be/bessadestr.c
	ir/be/bestack.c
	ir/be/bestat.c
	ir/be/bestate.c
	ir/be/betranshlp.c
	ir/be/beuses.c
	ir/be/beutil.c
	ir/be/bevarargs.c
	ir/be/beverify.c
	ir/be/ia32/bearch_ia32.c
	ir/be/ia32/ia32_architecture.c
	ir/be/ia32/ia32_cconv.c
	ir/be/ia32/ia32_emitter.c
	ir/be/ia32/ia32_finish.c
	ir/be/ia32/ia32_fpu.c
	ir/be/ia32/ia32_intrinsics.c
	ir/be/ia32/ia32_new_nodes.c
	ir/be/ia32/ia32_optimize.c
	ir/be/ia32/ia32_pic.c
	ir/be/ia32/ia32_transform.c
	ir/be/ia32/x86_x87.c
	ir/be/ia32/x86_address_mode.c
	ir/be/ia32/x86_asm.c
	ir/be/ia32/x86_cconv.c
	ir/be/ia32/x86_imm.c
	ir/be/sparc/bearch_sparc.c
	ir/be/sparc/sparc_cconv.c
	ir/be/sparc/sparc_emitter.c
	ir/be/sparc/sparc_finish.c
	ir/be/sparc/sparc_lower64.c
	ir/be/sparc/sparc_new_nodes.c
	ir/be/sparc/sparc_stackframe.c
	ir/be/sparc/sparc_transform.c
	ir/be/TEMPLATE/bearch_TEMPLATE.c
	ir/be/TEMPLATE/TEMPLATE_emitter.c
	ir/be/TEMPLATE/TEMPLATE_new_nodes.c
	ir/be/TEMPLATE/TEMPLATE_transform.c
	ir/common/debug.c
	ir/common/debugger.c
	ir/common/firm.c
	ir/common/firm_common.c
	ir/common/panic.c
	ir/common/timing.c
	ir/ident/ident.c
	ir/ir/dbginfo.c
	ir/ir/irarch.c
	ir/ir/irargs.c
	ir/ir/ircons.c
	ir/ir/irdump.c
	ir/ir/irdumptxt.c
	ir/ir/iredges.c
	ir/ir/irflag.c
	ir/ir/irgmod.c
	ir/ir/irgraph.c
	ir/ir/irgwalk.c
	ir/ir/irgwalk_blk.c
	ir/ir/irhooks.c
	ir/ir/irio.c
	ir/ir/irmode.c
	ir/ir/irnode.c
	ir/ir/irnodehashmap.c
	ir/ir/irnodeset.c
	ir/ir/irop.c
	ir/ir/irprintf.c
	ir/ir/irprofile.c
	ir/ir/irprog.c
	ir/ir/irssacons.c
	ir/ir/irtools.c
	ir/ir/irverify.c
	ir/ir/valueset.c
	ir/kaps/brute_force.c
	ir/kaps/bucket.c
	ir/kaps/heuristical.c
	ir/kaps/heuristical_co.c
	ir/kaps/heuristical_co_ld.c
	ir/kaps/html_dumper.c
	ir/kaps/kaps.c
	ir/kaps/matrix.c
	ir/kaps/optimal.c
	ir/kaps/pbqp_edge.c
	ir/kaps/pbqp_node.c
	ir/kaps/vector.c
	ir/libcore/lc_appendable.c
	ir/libcore/lc_opts.c
	ir/libcore/lc_opts_enum.c
	ir/libcore/lc_printf.c
	ir/lower/lower_alloc.c
	ir/lower/lower_builtins.c
	ir/lower/lower_calls.c
	ir/lower/lower_copyb.c
	ir/lower/lower_dw.c
	ir/lower/lower_hl.c
	ir/lower/lower_intrinsics.c
	ir/lower/lower_mode_b.c
	ir/lower/lower_mux.c
	ir/lower/lower_softfloat.c
	ir/lower/lower_switch.c
	ir/lpp/lpp.c
	ir/lpp/lpp_cplex.c
	ir/lpp/lpp_gurobi.c
	ir/lpp/lpp_solvers.c
	ir/lpp/mps.c
	ir/lpp/sp_matrix.c
	ir/obstack/obstack.c
	ir/obstack/obstack_printf.c
	ir/opt/boolopt.c
	ir/opt/cfopt.c
	ir/opt/code_placement.c
	ir/opt/combo.c
	ir/opt/convopt.c
	ir/opt/critical_edges.c
	ir/opt/dead_code_elimination.c
	ir/opt/funccall.c
	ir/opt/garbage_collect.c
	ir/opt/gvn_pre.c
	ir/opt/ifconv.c
	ir/opt/instrument.c
	ir/opt/ircgopt.c
	ir/opt/ircomplib.c
	ir/opt/irgopt.c
	ir/opt/iropt.c
	ir/opt/jumpthreading.c
	ir/opt/ldstopt.c
	ir/opt/loop.c
	ir/opt/occult_const.c
	ir/opt/opt_blocks.c
	ir/opt/opt_confirms.c
	ir/opt/opt_frame.c
	ir/opt/opt_inline.c
	ir/opt/opt_ldst.c
	ir/opt/opt_osr.c
	ir/opt/parallelize_mem.c
	ir/opt/proc_cloning.c
	ir/opt/reassoc.c
	ir/opt/return.c
	ir/opt/rm_bads.c
	ir/opt/rm_tuples.c
	ir/opt/scalar_replace.c
	ir/opt/tailrec.c
	ir/opt/unreachable.c
	ir/stat/stat_timing.c
	ir/stat/statev.c
	ir/tr/entity.c
	ir/tr/tpop.c
	ir/tr/tr_inheritance.c
	ir/tr/trverify.c
	ir/tr/type.c
	ir/tr/typewalk.c
	ir/tv/fltcalc.c
	ir/tv/strcalc.c
	ir/tv/tv.c
)
include_directories(
	include
	include/libfirm
	include/libfirm/adt
	ir/adt
	ir/ana
	ir/be
	ir/common
	ir/debug
	ir/ident
	ir/ir
	ir/kaps
	ir/libcore
	ir/lower
	ir/lpp
	ir/lower
	ir/obstack
	ir/opt
	ir/stat
	ir/tr
	ir/tv
)

# Codegenerators
set(GEN_DIR "${CMAKE_CURRENT_BINARY_DIR}/gen")
set(GEN_IR_DIR "${PROJECT_SOURCE_DIR}/scripts")
set(IR_SPEC "${PROJECT_SOURCE_DIR}/scripts/ir_spec.py")
set(GEN_TEMPLATEDIR "${PROJECT_SOURCE_DIR}/scripts/templates")

set(Python_ADDITIONAL_VERSIONS 2.7 3.0 3.1 3.2 3.3 3.4)
include(FindPythonInterp)
if(NOT PYTHONINTERP_FOUND)
	message(FATAL_ERROR "Unable to find python interpreter")
endif()
include(FindPerl)
if(NOT PERL_FOUND)
	message(FATAL_ERROR "Unable to find perl interpreter")
endif()

function(gen_ir target)
	get_filename_component(basename "${target}" NAME)
	add_custom_command (
		OUTPUT ${target}
		COMMAND ${PYTHON_EXECUTABLE} ${GEN_IR_DIR}/gen_ir.py ${IR_SPEC} ${GEN_TEMPLATEDIR}/${basename} > ${target}
		DEPENDS ${GEN_IR_DIR}/gen_ir.py ${GEN_IR_DIR}/jinjautil.py ${GEN_IR_DIR}/irops.py ${IR_SPEC}
	)
	list(APPEND SOURCES ${target})
	set(SOURCES ${SOURCES} PARENT_SCOPE)
endfunction()

function(begen generator target spec)
	get_filename_component(TARGETDIR "${target}" DIRECTORY)
	set(GENERATOR ${PROJECT_SOURCE_DIR}/ir/be/scripts/${generator})
	add_custom_command(
		OUTPUT ${target}
		COMMAND ${PERL_EXECUTABLE} ${GENERATOR} ${spec} ${TARGETDIR}
		DEPENDS ${GENERATOR} ${spec}
	)
	list(APPEND SOURCES ${target})
	set(SOURCES ${SOURCES} PARENT_SCOPE)
endfunction()

function(add_backend name)
	list(APPEND SOURCES ${ARGN})
	set(SPEC ${PROJECT_SOURCE_DIR}/ir/be/${name}/${name}_spec.pl)
	begen(generate_emitter.pl
		${GEN_DIR}/ir/be/${name}/gen_${name}_emitter.c
		${SPEC})
	begen(generate_regalloc_if.pl
		${GEN_DIR}/ir/be/${name}/gen_${name}_regalloc_if.c
		${SPEC})
	begen(generate_new_opcodes.pl
		${GEN_DIR}/ir/be/${name}/gen_${name}_new_nodes.c.inl
		${SPEC})
	set(SOURCES ${SOURCES} PARENT_SCOPE)
	include_directories(
		${PROJECT_SOURCE_DIR}/ir/be/${name}
		${GEN_DIR}/ir/be/${name}
	)
endfunction()

foreach(file
	include/libfirm/nodes.h
	ir/ir/gen_irnode.h
	ir/ir/gen_irdump.c.inl
	ir/ir/gen_irio.c.inl
	ir/ir/gen_irnode.c)
	gen_ir(${GEN_DIR}/${file})
endforeach(file)
include_directories(
	${GEN_DIR}/include/libfirm
	${GEN_DIR}/ir/ir
)

add_backend(ia32
	ir/be/ia32/bearch_ia32.c
	ir/be/ia32/ia32_architecture.c
	ir/be/ia32/ia32_cconv.c
	ir/be/ia32/ia32_emitter.c
	ir/be/ia32/ia32_finish.c
	ir/be/ia32/ia32_fpu.c
	ir/be/ia32/ia32_intrinsics.c
	ir/be/ia32/ia32_new_nodes.c
	ir/be/ia32/ia32_optimize.c
	ir/be/ia32/ia32_pic.c
	ir/be/ia32/ia32_transform.c
	ir/be/ia32/x86_address_mode.c
	ir/be/ia32/x86_asm.c
	ir/be/ia32/x86_cconv.c
	ir/be/ia32/x86_imm.c
	ir/be/ia32/x86_x87.c
)
add_backend(arm
	ir/be/arm/arm_cconv.c
	ir/be/arm/arm_emitter.c
	ir/be/arm/arm_finish.c
	ir/be/arm/arm_lower64.c
	ir/be/arm/arm_new_nodes.c
	ir/be/arm/arm_optimize.c
	ir/be/arm/arm_transform.c
	ir/be/arm/bearch_arm.c
)
add_backend(sparc
	ir/be/sparc/bearch_sparc.c
	ir/be/sparc/sparc_cconv.c
	ir/be/sparc/sparc_emitter.c
	ir/be/sparc/sparc_finish.c
	ir/be/sparc/sparc_lower64.c
	ir/be/sparc/sparc_new_nodes.c
	ir/be/sparc/sparc_stackframe.c
	ir/be/sparc/sparc_transform.c
)
add_backend(amd64
	ir/be/amd64/amd64_cconv.c
	ir/be/amd64/amd64_emitter.c
	ir/be/amd64/amd64_finish.c
	ir/be/amd64/amd64_new_nodes.c
	ir/be/amd64/amd64_transform.c
	ir/be/amd64/amd64_x87.c
	ir/be/amd64/bearch_amd64.c
)
add_backend(TEMPLATE
	ir/be/TEMPLATE/bearch_TEMPLATE.c
	ir/be/TEMPLATE/TEMPLATE_emitter.c
	ir/be/TEMPLATE/TEMPLATE_new_nodes.c
	ir/be/TEMPLATE/TEMPLATE_transform.c
)

add_library(firm SHARED ${SOURCES})

# Enable C99
if(CMAKE_VERSION VERSION_LESS "3.1")
	if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
		set(CMAKE_C_FLAGS "--std=gnu99 ${CMAKE_C_FLAGS}")
	endif()
else()
	set(CMAKE_C_STANDARD 99)
endif()
# Indicate that we build a shared library
add_definitions(-DFIRM_BUILD -DFIRM_DLL)

# Create install target
set(INSTALL_HEADERS
	include/libfirm/adt/array.h
	include/libfirm/adt/bipartite.h
	include/libfirm/adt/funcattr.h
	include/libfirm/adt/gaussjordan.h
	include/libfirm/adt/gaussseidel.h
	include/libfirm/adt/hashptr.h
	include/libfirm/adt/hungarian.h
	include/libfirm/adt/list.h
	include/libfirm/adt/obst.h
	include/libfirm/adt/obstack.h
	include/libfirm/adt/pdeq.h
	include/libfirm/adt/plist.h
	include/libfirm/adt/pmap.h
	include/libfirm/adt/pqueue.h
	include/libfirm/adt/pset.h
	include/libfirm/adt/set.h
	include/libfirm/adt/unionfind.h
	include/libfirm/adt/xmalloc.h
	include/libfirm/analyze_irg_args.h
	include/libfirm/be.h
	include/libfirm/begin.h
	include/libfirm/callgraph.h
	include/libfirm/cdep.h
	include/libfirm/cgana.h
	include/libfirm/dbginfo.h
	include/libfirm/end.h
	include/libfirm/execfreq.h
	include/libfirm/firm.h
	include/libfirm/firm_common.h
	include/libfirm/firm_types.h
	include/libfirm/heights.h
	include/libfirm/ident.h
	include/libfirm/irarch.h
	include/libfirm/ircgopt.h
	include/libfirm/ircons.h
	include/libfirm/irconsconfirm.h
	include/libfirm/irdom.h
	include/libfirm/irdump.h
	include/libfirm/iredgekinds.h
	include/libfirm/iredges.h
	include/libfirm/irflag.h
	include/libfirm/irgmod.h
	include/libfirm/irgopt.h
	include/libfirm/irgraph.h
	include/libfirm/irgwalk.h
	include/libfirm/irio.h
	include/libfirm/irloop.h
	include/libfirm/irmemory.h
	include/libfirm/irmode.h
	include/libfirm/irnode.h
	include/libfirm/irop.h
	include/libfirm/iropt.h
	include/libfirm/iroptimize.h
	include/libfirm/irouts.h
	include/libfirm/irprintf.h
	include/libfirm/irprog.h
	include/libfirm/irverify.h
	include/libfirm/lowering.h
	include/libfirm/statev.h
	include/libfirm/timing.h
	include/libfirm/tv.h
	include/libfirm/typerep.h
	include/libfirm/vrp.h
)

install(TARGETS firm
	EXPORT libfirm
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
)
foreach(header ${INSTALL_HEADERS})
	get_filename_component(DIR ${header} DIRECTORY)
	install(FILES ${header} DESTINATION ${DIR})
endforeach(header)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/gen/include/libfirm/nodes.h DESTINATION include/libfirm)

# Generate cmake configuration that external projects can include
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/libfirm-config.cmake
"add_library(firm SHARED IMPORTED)
find_library(FIRM_LIBRARY_PATH firm)
set_target_properties(firm PROPERTIES IMPORTED_LOCATION \"\${FIRM_LIBRARY_PATH}\")"
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libfirm-config.cmake DESTINATION lib/cmake/libfirm)

# Generate pkg-config for external projects
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/libfirm.pc
"prefix=${CMAKE_INSTALL_PREFIX}
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: ${PROJECT_NAME}
Description: ${PROJECT_DESCRIPTION}
Version: ${PROJECT_VERSION}
Requires:
Libs: -L\${libfir} -lfirm -lm
Cflags: -I\${includedir}"
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/libfirm.pc" DESTINATION lib/pkgconfig)
