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

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -DDEBUG_libfirm")

set(SOURCES
	src/adt/array.c
	src/adt/bipartite.c
	src/adt/bitset.c
	src/adt/cpset.c
	src/adt/deq.c
	src/adt/gaussjordan.c
	src/adt/gaussseidel.c
	src/adt/hungarian.c
	src/adt/pmap.c
	src/adt/pqueue.c
	src/adt/pset.c
	src/adt/pset_new.c
	src/adt/set.c
	src/adt/xmalloc.c
	src/ana/analyze_irg_args.c
	src/ana/callgraph.c
	src/ana/cdep.c
	src/ana/cgana.c
	src/ana/constbits.c
	src/ana/dca.c
	src/ana/dfs.c
	src/ana/domfront.c
	src/ana/execfreq.c
	src/ana/heights.c
	src/ana/irbackedge.c
	src/ana/ircfscc.c
	src/ana/irconsconfirm.c
	src/ana/irdom.c
	src/ana/irlivechk.c
	src/ana/irloop.c
	src/ana/irmemory.c
	src/ana/irouts.c
	src/ana/vrp.c
	src/be/be2addr.c
	src/be/bearch.c
	src/be/beasm.c
	src/be/beblocksched.c
	src/be/bechordal.c
	src/be/bechordal_common.c
	src/be/bechordal_main.c
	src/be/becopyheur4.c
	src/be/becopyilp.c
	src/be/becopyilp2.c
	src/be/becopyopt.c
	src/be/bediagnostic.c
	src/be/bedump.c
	src/be/bedwarf.c
	src/be/beemithlp.c
	src/be/beemitter.c
	src/be/beflags.c
	src/be/begnuas.c
	src/be/beifg.c
	src/be/beinfo.c
	src/be/beinsn.c
	src/be/beirg.c
	src/be/bejit.c
	src/be/belistsched.c
	src/be/belive.c
	src/be/beloopana.c
	src/be/belower.c
	src/be/bemain.c
	src/be/bemodule.c
	src/be/benode.c
	src/be/bepbqpcoloring.c
	src/be/bepeephole.c
	src/be/beprefalloc.c
	src/be/bera.c
	src/be/besched.c
	src/be/beschednormal.c
	src/be/beschedrand.c
	src/be/beschedtrivial.c
	src/be/bespill.c
	src/be/bespillbelady.c
	src/be/bespilldaemel.c
	src/be/bespillslots.c
	src/be/bespillutil.c
	src/be/bessaconstr.c
	src/be/bessadestr.c
	src/be/bestack.c
	src/be/bestat.c
	src/be/bestate.c
	src/be/betranshlp.c
	src/be/beuses.c
	src/be/beutil.c
	src/be/bevarargs.c
	src/be/beverify.c
	src/be/target.c
	src/be/machine_triple.c
	src/be/platform.c
	src/common/debug.c
	src/common/debugger.c
	src/common/firm.c
	src/common/firm_common.c
	src/common/panic.c
	src/common/timing.c
	src/ident/ident.c
	src/ir/dbginfo.c
	src/ir/irarch.c
	src/ir/irargs.c
	src/ir/ircons.c
	src/ir/irdump.c
	src/ir/irdumptxt.c
	src/ir/iredges.c
	src/ir/irflag.c
	src/ir/irgmod.c
	src/ir/irgraph.c
	src/ir/irgwalk.c
	src/ir/irgwalk_blk.c
	src/ir/irhooks.c
	src/ir/irio.c
	src/ir/irmode.c
	src/ir/irnode.c
	src/ir/irnodehashmap.c
	src/ir/irnodeset.c
	src/ir/irop.c
	src/ir/irprintf.c
	src/ir/irprofile.c
	src/ir/irprog.c
	src/ir/irssacons.c
	src/ir/irtools.c
	src/ir/irverify.c
	src/ir/valueset.c
	src/kaps/brute_force.c
	src/kaps/bucket.c
	src/kaps/heuristical.c
	src/kaps/heuristical_co.c
	src/kaps/heuristical_co_ld.c
	src/kaps/html_dumper.c
	src/kaps/kaps.c
	src/kaps/matrix.c
	src/kaps/optimal.c
	src/kaps/pbqp_edge.c
	src/kaps/pbqp_node.c
	src/kaps/vector.c
	src/libcore/lc_appendable.c
	src/libcore/lc_opts.c
	src/libcore/lc_opts_enum.c
	src/libcore/lc_printf.c
	src/lower/lower_alloc.c
	src/lower/lower_builtins.c
	src/lower/lower_calls.c
	src/lower/lower_copyb.c
	src/lower/lower_dw.c
	src/lower/lower_hl.c
	src/lower/lower_intrinsics.c
	src/lower/lower_mode_b.c
	src/lower/lower_mux.c
	src/lower/lower_softfloat.c
	src/lower/lower_switch.c
	src/lpp/lpp.c
	src/lpp/lpp_cplex.c
	src/lpp/lpp_gurobi.c
	src/lpp/lpp_solvers.c
	src/lpp/mps.c
	src/lpp/sp_matrix.c
	src/obstack/obstack.c
	src/obstack/obstack_printf.c
	src/opt/boolopt.c
	src/opt/cfopt.c
	src/opt/code_placement.c
	src/opt/combo.c
	src/opt/convopt.c
	src/opt/critical_edges.c
	src/opt/dead_code_elimination.c
	src/opt/funccall.c
	src/opt/garbage_collect.c
	src/opt/gvn_pre.c
	src/opt/ifconv.c
	src/opt/instrument.c
	src/opt/ircgopt.c
	src/opt/ircomplib.c
	src/opt/irgopt.c
	src/opt/iropt.c
	src/opt/jumpthreading.c
	src/opt/ldstopt.c
	src/opt/loop.c
	src/opt/lcssa.c
	src/opt/loop_unrolling.c
	src/opt/occult_const.c
	src/opt/opt_blocks.c
	src/opt/opt_confirms.c
	src/opt/opt_frame.c
	src/opt/opt_inline.c
	src/opt/opt_ldst.c
	src/opt/opt_osr.c
	src/opt/parallelize_mem.c
	src/opt/proc_cloning.c
	src/opt/reassoc.c
	src/opt/return.c
	src/opt/rm_bads.c
	src/opt/rm_tuples.c
	src/opt/scalar_replace.c
	src/opt/tailrec.c
	src/opt/unreachable.c
	src/stat/stat_timing.c
	src/stat/statev.c
	src/tr/entity.c
	src/tr/tr_inheritance.c
	src/tr/trverify.c
	src/tr/type.c
	src/tr/typewalk.c
	src/tv/fltcalc.c
	src/tv/strcalc.c
	src/tv/tv.c
)
include_directories(
	include
	include/libfirm
	include/libfirm/adt
	src/adt
	src/ana
	src/be
	src/common
	src/debug
	src/ident
	src/ir
	src/kaps
	src/libcore
	src/lower
	src/lpp
	src/lower
	src/obstack
	src/opt
	src/stat
	src/tr
	src/tv
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
	set(GENERATOR ${PROJECT_SOURCE_DIR}/src/be/scripts/${generator})
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
	set(SPEC ${PROJECT_SOURCE_DIR}/src/be/${name}/${name}_spec.pl)
	begen(generate_emitter.pl
		${GEN_DIR}/src/be/${name}/gen_${name}_emitter.c
		${SPEC})
	begen(generate_regalloc_if.pl
		${GEN_DIR}/src/be/${name}/gen_${name}_regalloc_if.c
		${SPEC})
	begen(generate_new_opcodes.pl
		${GEN_DIR}/src/be/${name}/gen_${name}_new_nodes.c
		${SPEC})
	set(SOURCES ${SOURCES} PARENT_SCOPE)
	include_directories(
		${PROJECT_SOURCE_DIR}/src/be/${name}
		${GEN_DIR}/src/be/${name}
	)
endfunction()

foreach(file
	include/libfirm/nodes.h
	src/ir/gen_irnode.h
	src/ir/gen_proj_names.h
	src/ir/gen_irio.c
	src/ir/gen_irnode.c)
	gen_ir(${GEN_DIR}/${file})
endforeach(file)
include_directories(
	${GEN_DIR}/include/libfirm
	${GEN_DIR}/src/ir
)

add_backend(ia32
	src/be/ia32/ia32_architecture.c
	src/be/ia32/ia32_bearch.c
	src/be/ia32/ia32_cconv.c
	src/be/ia32/ia32_emitter.c
	src/be/ia32/ia32_encode.c
	src/be/ia32/ia32_finish.c
	src/be/ia32/ia32_fpu.c
	src/be/ia32/ia32_intrinsics.c
	src/be/ia32/ia32_new_nodes.c
	src/be/ia32/ia32_optimize.c
	src/be/ia32/ia32_pic.c
	src/be/ia32/ia32_transform.c
	src/be/ia32/x86_address_mode.c
	src/be/ia32/x86_architecture.c
	src/be/ia32/x86_asm.c
	src/be/ia32/x86_cconv.c
	src/be/ia32/x86_node.c
	src/be/ia32/x86_x87.c
)
add_backend(arm
	src/be/arm/arm_bearch.c
	src/be/arm/arm_cconv.c
	src/be/arm/arm_emitter.c
	src/be/arm/arm_finish.c
	src/be/arm/arm_lower64.c
	src/be/arm/arm_new_nodes.c
	src/be/arm/arm_optimize.c
	src/be/arm/arm_transform.c
)
add_backend(sparc
	src/be/sparc/sparc_bearch.c
	src/be/sparc/sparc_cconv.c
	src/be/sparc/sparc_emitter.c
	src/be/sparc/sparc_finish.c
	src/be/sparc/sparc_lower64.c
	src/be/sparc/sparc_new_nodes.c
	src/be/sparc/sparc_stackframe.c
	src/be/sparc/sparc_transform.c
)
add_backend(amd64
	src/be/amd64/amd64_architecture.c
	src/be/amd64/amd64_bearch.c
	src/be/amd64/amd64_cconv.c
	src/be/amd64/amd64_emitter.c
	src/be/amd64/amd64_finish.c
	src/be/amd64/amd64_new_nodes.c
	src/be/amd64/amd64_optimize.c
	src/be/amd64/amd64_pic.c
	src/be/amd64/amd64_transform.c
	src/be/amd64/amd64_varargs.c
	src/be/amd64/amd64_x87.c
	src/be/amd64/amd64_abi.c
)
add_backend(mips
	src/be/mips/mips_bearch.c
	src/be/mips/mips_bearch_t.h
	src/be/mips/mips_cconv.c
	src/be/mips/mips_cconv.h
	src/be/mips/mips_emitter.c
	src/be/mips/mips_emitter.h
	src/be/mips/mips_lower64.c
	src/be/mips/mips_lower64.h
	src/be/mips/mips_new_nodes.c
	src/be/mips/mips_new_nodes_t.h
	src/be/mips/mips_nodes_attr.c
	src/be/mips/mips_nodes_attr.h
	src/be/mips/mips_spec.pl
	src/be/mips/mips_transform.c
	src/be/mips/mips_transform.h
)
add_backend(riscv
	src/be/riscv/riscv_abi.c
	src/be/riscv/riscv_bearch.c
	src/be/riscv/riscv_cconv.c
	src/be/riscv/riscv_emitter.c
	src/be/riscv/riscv_finish.c
	src/be/riscv/riscv_lower64.c
	src/be/riscv/riscv_new_nodes.c
	src/be/riscv/riscv_nodes_attr.c
	src/be/riscv/riscv_transform.c
)
add_backend(TEMPLATE
	src/be/TEMPLATE/TEMPLATE_bearch.c
	src/be/TEMPLATE/TEMPLATE_emitter.c
	src/be/TEMPLATE/TEMPLATE_new_nodes.c
	src/be/TEMPLATE/TEMPLATE_transform.c
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
elseif(WIN32 OR MINGW)
	target_link_libraries(firm LINK_PUBLIC regex winmm)
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
