cmake_minimum_required(VERSION 2.8.12)
if(POLICY CMP0042)
	cmake_policy(SET CMP0042 NEW)
endif()
if(POLICY CMP0063)
	cmake_policy(SET CMP0063 OLD)
endif()

project(libfirm C)
set(libfirm_VERSION "1.22.1")
set(PROJECT_DESCRIPTION "library implementing the intermediate representation Firm")

set(CMAKE_C_VISIBILITY_PRESET hidden)

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DDEBUG_libfirm")

set(SOURCES
	ir/adt/array.c
	ir/adt/bipartite.c
	ir/adt/bitset.c
	ir/adt/cpset.c
	ir/adt/deq.c
	ir/adt/gaussjordan.c
	ir/adt/gaussseidel.c
	ir/adt/hungarian.c
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
	ir/be/be2addr.c
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
	ir/be/beemithlp.c
	ir/be/beemitter.c
	ir/be/beflags.c
	ir/be/begnuas.c
	ir/be/beifg.c
	ir/be/beinfo.c
	ir/be/beinsn.c
	ir/be/beirg.c
	ir/be/bejit.c
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
	ir/be/target.c
	ir/be/machine_triple.c
	ir/be/platform.c
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
	ir/opt/lcssa.c
	ir/opt/loop_unrolling.c
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

set(TESTS
	unittests/deq
	unittests/globalmap
	unittests/nan_payload
	unittests/rbitset
	unittests/sc_val_from_bits
	unittests/snprintf
	unittests/strcalc
	unittests/tarval_calc
	unittests/tarval_float
	unittests/tarval_floatops
	unittests/tarval_from_to
	unittests/tarval_is_long
)

# Codegenerators
#
# If you change GEN_DIR, be sure to adjust cparser's CMakeLists accordingly.
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
	get_filename_component(basename ${target} NAME)
	get_filename_component(target_dir ${target} DIRECTORY)
	add_custom_command (
		OUTPUT ${target}
		COMMAND ${CMAKE_COMMAND} -E make_directory ${target_dir}
		COMMAND ${PYTHON_EXECUTABLE} ${GEN_IR_DIR}/gen_ir.py ${IR_SPEC} ${GEN_TEMPLATEDIR}/${basename} > ${target}
		DEPENDS ${GEN_IR_DIR}/gen_ir.py ${GEN_IR_DIR}/jinjautil.py ${GEN_IR_DIR}/irops.py ${IR_SPEC}
	)
	list(APPEND SOURCES ${target})
	set(SOURCES ${SOURCES} PARENT_SCOPE)
endfunction()

function(begen generator target spec)
	get_filename_component(target_dir ${target} DIRECTORY)
	set(GENERATOR ${PROJECT_SOURCE_DIR}/ir/be/scripts/${generator})
	add_custom_command(
		OUTPUT ${target}
		COMMAND ${CMAKE_COMMAND} -E make_directory ${target_dir}
		COMMAND ${PERL_EXECUTABLE} ${GENERATOR} ${spec} ${target_dir}
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
		${GEN_DIR}/ir/be/${name}/gen_${name}_new_nodes.c
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
	ir/ir/gen_proj_names.h
	ir/ir/gen_irio.c
	ir/ir/gen_irnode.c)
	gen_ir(${GEN_DIR}/${file})
endforeach(file)
include_directories(
	${GEN_DIR}/include/libfirm
	${GEN_DIR}/ir/ir
)

add_backend(ia32
	ir/be/ia32/ia32_architecture.c
	ir/be/ia32/ia32_bearch.c
	ir/be/ia32/ia32_cconv.c
	ir/be/ia32/ia32_emitter.c
	ir/be/ia32/ia32_encode.c
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
	ir/be/ia32/x86_node.c
	ir/be/ia32/x86_x87.c
)
add_backend(arm
	ir/be/arm/arm_bearch.c
	ir/be/arm/arm_cconv.c
	ir/be/arm/arm_emitter.c
	ir/be/arm/arm_finish.c
	ir/be/arm/arm_lower64.c
	ir/be/arm/arm_new_nodes.c
	ir/be/arm/arm_optimize.c
	ir/be/arm/arm_transform.c
)
add_backend(sparc
	ir/be/sparc/sparc_bearch.c
	ir/be/sparc/sparc_cconv.c
	ir/be/sparc/sparc_emitter.c
	ir/be/sparc/sparc_finish.c
	ir/be/sparc/sparc_lower64.c
	ir/be/sparc/sparc_new_nodes.c
	ir/be/sparc/sparc_stackframe.c
	ir/be/sparc/sparc_transform.c
)
add_backend(amd64
	ir/be/amd64/amd64_bearch.c
	ir/be/amd64/amd64_cconv.c
	ir/be/amd64/amd64_emitter.c
	ir/be/amd64/amd64_finish.c
	ir/be/amd64/amd64_new_nodes.c
	ir/be/amd64/amd64_optimize.c
	ir/be/amd64/amd64_pic.c
	ir/be/amd64/amd64_transform.c
	ir/be/amd64/amd64_varargs.c
	ir/be/amd64/amd64_x87.c
	ir/be/amd64/amd64_abi.c
)
add_backend(mips
	ir/be/mips/mips_bearch.c
	ir/be/mips/mips_bearch_t.h
	ir/be/mips/mips_cconv.c
	ir/be/mips/mips_cconv.h
	ir/be/mips/mips_emitter.c
	ir/be/mips/mips_emitter.h
	ir/be/mips/mips_lower64.c
	ir/be/mips/mips_lower64.h
	ir/be/mips/mips_new_nodes.c
	ir/be/mips/mips_new_nodes_t.h
	ir/be/mips/mips_nodes_attr.c
	ir/be/mips/mips_nodes_attr.h
	ir/be/mips/mips_spec.pl
	ir/be/mips/mips_transform.c
	ir/be/mips/mips_transform.h
)
add_backend(riscv
	ir/be/riscv/riscv_bearch.c
	ir/be/riscv/riscv_cconv.c
	ir/be/riscv/riscv_emitter.c
	ir/be/riscv/riscv_finish.c
	ir/be/riscv/riscv_lower64.c
	ir/be/riscv/riscv_new_nodes.c
	ir/be/riscv/riscv_nodes_attr.c
	ir/be/riscv/riscv_transform.c
)
add_backend(TEMPLATE
	ir/be/TEMPLATE/TEMPLATE_bearch.c
	ir/be/TEMPLATE/TEMPLATE_emitter.c
	ir/be/TEMPLATE/TEMPLATE_new_nodes.c
	ir/be/TEMPLATE/TEMPLATE_transform.c
)

# Produce revision.h
include_directories(${CMAKE_CURRENT_BINARY_DIR})
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git/HEAD)
	set(REVISIONH "${CMAKE_CURRENT_BINARY_DIR}/firm_revision.h")
	set(REVGEN echo '\#define libfirm_VERSION_REVISION \"'`git describe --abbrev=40 --always --dirty --match ''`'\"')
	add_custom_command(
		OUTPUT ${REVISIONH}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/.git/HEAD
		COMMAND ${REVGEN} | cmp -s - "${REVISIONH}" 2> /dev/null || ${REVGEN} > "${REVISIONH}"
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
	SET_SOURCE_FILES_PROPERTIES(ir/common/firm.c PROPERTIES OBJECT_DEPENDS ${REVISIONH})
	add_definitions(-DHAVE_FIRM_REVISION_H)
endif()

# Enable C99
if(CMAKE_VERSION VERSION_LESS "3.1")
	if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
		set(CMAKE_C_FLAGS "-std=c99 ${CMAKE_C_FLAGS}")
	endif()
else()
	set(CMAKE_C_STANDARD 99)
endif()
# Indicate that we build a shared library
add_definitions(-DFIRM_BUILD -DFIRM_DLL)

# Build library
set(BUILD_SHARED_LIBS Off CACHE BOOL "whether to build shared libraries")
add_library(firm ${SOURCES})
if(UNIX)
	target_link_libraries(firm LINK_PUBLIC m)
elseif(WIN32)
	target_link_libraries(firm LINK_PUBLIC gnurx winmm)
endif()

enable_testing()
add_custom_target(
		check
		${CMAKE_CTEST_COMMAND} -C $<CONFIG>
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
foreach(test ${TESTS})
	# Replace `/` with a `.` so cmake accepts the target name
	string(REPLACE "/" "." test-id ${test})
	add_executable(${test-id} ${test}.c)
	target_link_libraries(${test-id} LINK_PRIVATE firm)
	add_test(test-${test-id} ${test-id})
	add_dependencies(check ${test-id})
endforeach(test)

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
	EXPORT libfirmTargets
	DESTINATION lib
)
foreach(header ${INSTALL_HEADERS})
	get_filename_component(DIR ${header} DIRECTORY)
	install(FILES ${header} DESTINATION ${DIR})
endforeach(header)
install(FILES ${GEN_DIR}/include/libfirm/nodes.h DESTINATION include/libfirm)

# Generate cmake Config file
include(CMakePackageConfigHelpers)
set(libfirm_INCLUDE_DIRS include)
set(ConfigPackageLocation lib/cmake/libfirm)
configure_package_config_file(support/libfirmConfig.cmake.in
	${CMAKE_CURRENT_BINARY_DIR}/libfirmConfig.cmake
	INSTALL_DESTINATION ${ConfigPackageLocation}
	PATH_VARS libfirm_INCLUDE_DIRS
)
write_basic_package_version_file(
	${CMAKE_CURRENT_BINARY_DIR}/libfirmConfigVersion.cmake
	VERSION ${libfirm_VERSION}
	COMPATIBILITY ExactVersion
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libfirmConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/libfirmConfigVersion.cmake
        DESTINATION ${ConfigPackageLocation}
)
install(EXPORT libfirmTargets
	FILE libfirmTargets.cmake
	NAMESPACE libfirm::
	DESTINATION ${ConfigPackageLocation}
)

# Generate pkg-config for external projects
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/support/libfirm.pc.in
	${CMAKE_CURRENT_BINARY_DIR}/libfirm.pc
	@ONLY
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/libfirm.pc"
	DESTINATION lib/pkgconfig
)
