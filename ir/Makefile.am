lib_LTLIBRARIES = libfirm.la

INCLUDES = -I$(top_srcdir)/include/libfirm \
	-I$(top_srcdir)/include/libfirm/adt \
	-I$(top_builddir)

INCLUDES += \
	-I$(top_srcdir)/ir/adt \
	-I$(top_srcdir)/ir/ana \
	-I$(top_srcdir)/ir/arch \
	-I$(top_srcdir)/ir/be \
	-I$(top_srcdir)/ir/common \
	-I$(top_srcdir)/ir/debug \
	-I$(top_srcdir)/ir/ident \
	-I$(top_srcdir)/ir/ir \
	-I$(top_srcdir)/ir/kaps \
	-I$(top_srcdir)/ir/libcore \
	-I$(top_srcdir)/ir/lower \
	-I$(top_srcdir)/ir/lpp \
	-I$(top_srcdir)/ir/net \
	-I$(top_srcdir)/ir/opt \
	-I$(top_srcdir)/ir/stat \
	-I$(top_srcdir)/ir/tr \
	-I$(top_srcdir)/ir/tv

libfirm_la_LDFLAGS = -no-undefined -version-info $(LT_VERSION) -lm
libfirm_la_CFLAGS = -std=c99
libfirm_la_SOURCES = \
	adt/array.c \
	adt/bipartite.c \
	adt/cpset.c \
	adt/gaussjordan.c \
	adt/gaussseidel.c \
	adt/hungarian.c \
	adt/pdeq.c \
	adt/plist.c \
	adt/pmap.c \
	adt/pqueue.c \
	adt/pset.c \
	adt/pset_new.c \
	adt/set.c \
	adt/xmalloc.c \
	ana/analyze_irg_args.c \
	ana/callgraph.c \
	ana/cdep.c \
	ana/cgana.c \
	ana/dfs.c \
	ana/domfront.c \
	ana/execfreq.c \
	ana/heights.c \
	ana/irbackedge.c \
	ana/ircfscc.c \
	ana/irconsconfirm.c \
	ana/irdom.c \
	ana/irlivechk.c \
	ana/irloop.c \
	ana/irmemory.c \
	ana/irouts.c \
	ana/irscc.c \
	ana/vrp.c \
	be/beabi.c \
	be/beabihelper.c \
	be/bearch.c \
	be/beblocksched.c \
	be/bechordal.c \
	be/bechordal_common.c \
	be/bechordal_draw.c \
	be/bechordal_main.c \
	be/becopyheur2.c \
	be/becopyheur4.c \
	be/becopyheur.c \
	be/becopyilp2.c \
	be/becopyilp.c \
	be/becopyopt.c \
	be/becopystat.c \
	be/bedump.c \
	be/bedwarf.c \
	be/beemitter_binary.c \
	be/beemitter.c \
	be/beflags.c \
	be/begnuas.c \
	be/beifg.c \
	be/beinfo.c \
	be/beinsn.c \
	be/beirg.c \
	be/beirgmod.c \
	be/belistsched.c \
	be/belive.c \
	be/beloopana.c \
	be/belower.c \
	be/bemain.c \
	be/bemodule.c \
	be/benode.c \
	be/bepbqpcoloring.c \
	be/bepeephole.c \
	be/beprefalloc.c \
	be/bera.c \
	be/besched.c \
	be/beschednormal.c \
	be/beschedrand.c \
	be/beschedregpress.c \
	be/beschedtrace.c \
	be/beschedtrivial.c \
	be/bespillbelady.c \
	be/bespill.c \
	be/bespilldaemel.c \
	be/bespillslots.c \
	be/bespillutil.c \
	be/bessaconstr.c \
	be/bessadestr.c \
	be/bestack.c \
	be/bestat.c \
	be/bestate.c \
	be/betranshlp.c \
	be/beuses.c \
	be/beutil.c \
	be/beverify.c \
	common/debug.c \
	common/error.c \
	common/firm.c \
	common/firm_common.c \
	common/irtools.c \
	common/timing.c \
	debug/dbginfo.c \
	debug/debugger.c \
	ident/ident.c \
	ident/mangle.c \
	ir/instrument.c \
	ir/irarch.c \
	ir/irargs.c \
	ir/ircomplib.c \
	ir/ircons.c \
	ir/irdump.c \
	ir/irdumptxt.c \
	ir/iredges.c \
	ir/irflag.c \
	ir/irgmod.c \
	ir/irgopt.c \
	ir/irgraph.c \
	ir/irgwalk_blk.c \
	ir/irgwalk.c \
	ir/irhooks.c \
	ir/irio.c \
	ir/irmode.c \
	ir/irnode.c \
	ir/irnodehashmap.c \
	ir/irnodeset.c \
	ir/irop.c \
	ir/iropt.c \
	ir/irpass.c \
	ir/irprintf.c \
	ir/irprofile.c \
	ir/irprog.c \
	ir/irssacons.c \
	ir/irverify.c \
	ir/rm_bads.c \
	ir/rm_tuples.c \
	ir/unreachable.c \
	ir/valueset.c \
	libcore/lc_appendable.c \
	libcore/lc_opts.c \
	libcore/lc_opts_enum.c \
	libcore/lc_printf.c \
	lower/lower_alloc.c \
	lower/lower_builtins.c \
	lower/lower_calls.c \
	lower/lower_copyb.c \
	lower/lower_dw.c \
	lower/lower_hl.c \
	lower/lower_intrinsics.c \
	lower/lower_mode_b.c \
	lower/lower_mux.c \
	lower/lower_softfloat.c \
	lower/lower_switch.c \
	lpp/lpp.c \
	lpp/lpp_comm.c \
	lpp/lpp_cplex.c \
	lpp/lpp_gurobi.c \
	lpp/lpp_net.c \
	lpp/lpp_solvers.c \
	lpp/mps.c \
	lpp/sp_matrix.c \
	obstack/obstack.c \
	obstack/obstack_printf.c \
	opt/boolopt.c \
	opt/cfopt.c \
	opt/code_placement.c \
	opt/combo.c \
	opt/convopt.c \
	opt/critical_edges.c \
	opt/dead_code_elimination.c \
	opt/fp-vrp.c \
	opt/funccall.c \
	opt/garbage_collect.c \
	opt/gvn_pre.c \
	opt/ifconv.c \
	opt/ircgopt.c \
	opt/jumpthreading.c \
	opt/ldstopt.c \
	opt/loop.c \
	opt/opt_blocks.c \
	opt/opt_confirms.c \
	opt/opt_frame.c \
	opt/opt_inline.c \
	opt/opt_ldst.c \
	opt/opt_osr.c \
	opt/parallelize_mem.c \
	opt/proc_cloning.c \
	opt/reassoc.c \
	opt/return.c \
	opt/scalar_replace.c \
	opt/tailrec.c \
	opt/tropt.c \
	stat/const_stat.c \
	stat/dags.c \
	stat/distrib.c \
	stat/firmstat.c \
	stat/pattern.c \
	stat/pattern_dmp.c \
	stat/stat_dmp.c \
	stat/statev.c \
	stat/stat_timing.c \
	tr/entity.c \
	tr/tpop.c \
	tr/tr_inheritance.c \
	tr/trverify.c \
	tr/type.c \
	tr/type_finalization.c \
	tr/typewalk.c \
	tv/fltcalc.c \
	tv/strcalc.c \
	tv/tv.c \
	kaps/brute_force.c \
	kaps/bucket.c \
	kaps/heuristical.c \
	kaps/heuristical_co.c \
	kaps/heuristical_co_ld.c \
	kaps/html_dumper.c \
	kaps/kaps.c \
	kaps/matrix.c \
	kaps/optimal.c \
	kaps/pbqp_edge.c \
	kaps/pbqp_node.c \
	kaps/vector.c

EXTRA_DIST = \
	adt/array_t.h \
	adt/bitfiddle.h \
	adt/bitset.h \
	adt/util.h \
	adt/compiler.h \
	adt/cpset.h \
	adt/fourcc.h \
	adt/raw_bitset.h \
	adt/hashset.h \
	adt/pset_new.h \
	ana/dfs.h \
	ana/dfs_t.h \
	ana/irbackedge_t.h \
	ana/irdom_t.h \
	ana/cdep_t.h \
	ana/irlivechk.h \
	ana/irloop_t.h \
	ana/irmemory_t.h \
	ana/execfreq_t.h \
	be/be_t.h \
	be/be_types.h \
	be/beabi.h \
	be/bearch.h \
	be/beblocksched.h \
	be/bechordal.h \
	be/bechordal_draw.h \
	be/bechordal_t.h \
	be/becopyopt_t.h \
	be/beabihelper.h \
	be/becopyilp_t.h \
	be/becopyopt.h \
	be/becopystat.h \
	be/bedump.h \
	be/belive.h \
	be/beintlive_t.h \
	be/beemitter.h \
	be/begnuas.h \
	be/beifg.h \
	be/bechordal_common.h \
	be/beinsn_t.h \
	be/beirg.h \
	be/beflags.h \
	be/beirgmod.h \
	be/beemitter_binary.h \
	be/belistsched.h \
	be/belive_t.h \
	be/beloopana.h \
	be/belower.h \
	be/beinfo.h \
	be/bessaconstr.h \
	be/bemodule.h \
	be/bemodule_t.h \
	be/benode.h \
	be/bepeephole.h \
	be/bepbqpcoloring.h \
	be/bera.h \
	be/besched.h \
	be/bespill.h \
	be/bespillslots.h \
	be/beutil.h \
	be/bessadestr.h \
	be/bespillutil.h \
	be/bestat.h \
	be/betranshlp.h \
	be/bestack.h \
	be/bestate.h \
	be/beverify.h \
	be/bedwarf.h \
	be/beuses.h \
	be/bedwarf_t.h \
	common/debug.h \
	common/error.h \
	common/irtools.h \
	debug/dbginfo_t.h \
	debug/debugger.h \
	ident/ident_t.h \
	ir/irflag_t.def \
	ir/irargs_t.h \
	ir/instrument.h \
	ir/ircons_t.h \
	ir/irdump_t.h \
	ir/iredges_t.h \
	ir/irflag_t.h \
	ir/irgraph_t.h \
	ir/iredgeset.h \
	ir/irmode_t.h \
	ir/irnode_t.h \
	ir/irnodeset.h \
	ir/gen_irnode.h \
	ir/irop_t.h \
	ir/iropt_t.h \
	ir/irprog_t.h \
	ir/irnodemap.h \
	ir/irpass_t.h \
	ir/irnodehashmap.h \
	ir/irtypes.h \
	ir/irverify_t.h \
	ir/irprofile.h \
	ir/valueset.h \
	libcore/lc_appendable.h \
	libcore/lc_opts.h \
	libcore/lc_opts_enum.h \
	libcore/lc_opts_t.h \
	libcore/lc_printf.h \
	libcore/lc_printf_arg_types.def \
	lower/lower_alloc.h \
	lower/lower_builtins.h \
	lower/lower_calls.h \
	lower/lower_dw.h \
	lower/lower_mode_b.h \
	lower/lower_softfloat.h \
	lpp/lpp.h \
	lpp/lpp_comm.h \
	lpp/lpp_cplex.h \
	lpp/lpp_gurobi.h \
	lpp/lpp_net.h \
	lpp/lpp_solvers.h \
	lpp/lpp_t.h \
	lpp/mps.h \
	lpp/sp_matrix.h \
	opt/iropt_dbg.h \
	opt/reassoc_t.h \
	opt/scalar_replace.h \
	opt/opt_init.h \
	stat/counter.h \
	stat/dags.h \
	stat/firmstat_t.h \
	stat/pattern.h \
	stat/pattern_dmp.h \
	stat/stat_dmp.h \
	stat/stat_timing.h \
	stat/statev_t.h \
	tr/entity_t.h \
	tr/tpop_t.h \
	tr/type_t.h \
	tv/fltcalc.h \
	tv/tv_t.h \
	tv/strcalc.h \
	kaps/brute_force.h \
	kaps/bucket.h \
	kaps/bucket_t.h \
	kaps/heuristical.h \
	kaps/heuristical_co.h \
	kaps/heuristical_co_ld.h \
	kaps/html_dumper.h \
	kaps/kaps.h \
	kaps/matrix.h \
	kaps/matrix_t.h \
	kaps/optimal.h \
	kaps/pbqp_edge.h \
	kaps/pbqp_edge_t.h \
	kaps/pbqp_node.h \
	kaps/pbqp_node_t.h \
	kaps/pbqp_t.h \
	kaps/vector.h \
	kaps/vector_t.h \
	ir/gen_ir_cons.c.inl \
	ir/gen_irop.c.inl \
	ir/gen_irio.inl \
	ir/gen_irnode.c.inl \
	ir/gen_irdump.c.inl \
	adt/hashset.c.inl \
	be/scripts/generate_emitter.pl \
	be/scripts/generate_new_opcodes.pl \
	be/scripts/generate_regalloc_if.pl

# ia32 backend
ia32_sources = \
	be/ia32/bearch_ia32.c \
	be/ia32/ia32_address_mode.c \
	be/ia32/ia32_architecture.c \
	be/ia32/ia32_common_transform.c \
	be/ia32/ia32_emitter.c \
	be/ia32/ia32_finish.c \
	be/ia32/ia32_fpu.c \
	be/ia32/ia32_intrinsics.c \
	be/ia32/ia32_new_nodes.c \
	be/ia32/ia32_optimize.c \
	be/ia32/ia32_transform.c \
	be/ia32/ia32_x87.c

ia32_built_sources = \
	be/ia32/gen_ia32_emitter.c \
	be/ia32/gen_ia32_emitter.h \
	be/ia32/gen_ia32_new_nodes.c.inl \
	be/ia32/gen_ia32_new_nodes.h \
	be/ia32/gen_ia32_regalloc_if.c \
	be/ia32/gen_ia32_regalloc_if.h
MAINTAINERCLEANFILES = $(ia32_built_sources)
BUILT_SOURCES        = $(ia32_built_sources)
libfirm_la_SOURCES  += $(ia32_sources) $(ia32_built_sources)

EXTRA_DIST += \
	be/ia32/bearch_ia32_t.h \
	be/ia32/ia32_address_mode.h \
	be/ia32/ia32_architecture.h \
	be/ia32/ia32_common_transform.h \
	be/ia32/ia32_emitter.h \
	be/ia32/ia32_finish.h \
	be/ia32/ia32_fpu.h \
	be/ia32/ia32_new_nodes.h \
	be/ia32/ia32_nodes_attr.h \
	be/ia32/ia32_optimize.h \
	be/ia32/ia32_spec.pl \
	be/ia32/ia32_transform.h \
	be/ia32/ia32_x87.h

$(srcdir)/be/ia32/gen_ia32_new_nodes.c.inl $(srcdir)/be/ia32/gen_ia32_new_nodes.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

$(srcdir)/be/ia32/gen_ia32_emitter.c $(srcdir)/be/ia32/gen_ia32_emitter.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

$(srcdir)/be/ia32/gen_ia32_regalloc_if.c $(srcdir)/be/ia32/gen_ia32_regalloc_if.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

# arm backend

arm_sources = \
	be/arm/arm_emitter.c \
	be/arm/arm_new_nodes.c \
	be/arm/arm_optimize.c \
	be/arm/arm_transform.c \
	be/arm/arm_map_regs.c \
	be/arm/arm_cconv.c \
	be/arm/bearch_arm.c
arm_built_sources = \
	be/arm/gen_arm_emitter.c \
	be/arm/gen_arm_emitter.h \
	be/arm/gen_arm_new_nodes.c.inl \
	be/arm/gen_arm_new_nodes.h \
	be/arm/gen_arm_regalloc_if.c \
	be/arm/gen_arm_regalloc_if.h
MAINTAINERCLEANFILES += $(arm_built_sources)
BUILT_SOURCES += $(arm_built_sources)
libfirm_la_SOURCES += $(arm_sources) $(arm_built_sources)


EXTRA_DIST += \
	be/arm/arm_emitter.h \
	be/arm/arm_new_nodes.h \
	be/arm/arm_nodes_attr.h \
	be/arm/arm_optimize.h \
	be/arm/arm_spec.pl \
	be/arm/arm_transform.h \
	be/arm/arm_map_regs.h \
	be/arm/arm_cconv.h \
	be/arm/bearch_arm_t.h

$(srcdir)/be/arm/gen_arm_new_nodes.c.inl $(srcdir)/be/arm/gen_arm_new_nodes.h: \
		be/arm/arm_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

$(srcdir)/be/arm/gen_arm_emitter.c $(srcdir)/be/arm/gen_arm_emitter.h: \
		be/arm/arm_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

$(srcdir)/be/arm/gen_arm_regalloc_if.c $(srcdir)/be/arm/gen_arm_regalloc_if.h: \
		be/arm/arm_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

# sparc backend

sparc_sources = \
	be/sparc/bearch_sparc.c \
	be/sparc/sparc_cconv.c \
	be/sparc/sparc_emitter.c \
	be/sparc/sparc_finish.c \
	be/sparc/sparc_lower64.c \
	be/sparc/sparc_new_nodes.c \
	be/sparc/sparc_stackframe.c \
	be/sparc/sparc_transform.c
sparc_built_sources = \
	be/sparc/gen_sparc_emitter.c \
	be/sparc/gen_sparc_emitter.h \
	be/sparc/gen_sparc_new_nodes.c.inl \
	be/sparc/gen_sparc_new_nodes.h \
	be/sparc/gen_sparc_regalloc_if.c \
	be/sparc/gen_sparc_regalloc_if.h
MAINTAINERCLEANFILES += $(sparc_built_sources)
BUILT_SOURCES += $(sparc_built_sources)
libfirm_la_SOURCES += $(sparc_sources) $(sparc_built_sources)

EXTRA_DIST += \
	be/sparc/sparc_emitter.h \
	be/sparc/sparc_new_nodes.h \
	be/sparc/sparc_nodes_attr.h \
	be/sparc/sparc_spec.pl \
	be/sparc/sparc_cconv.h \
	be/sparc/sparc_transform.h \
	be/sparc/bearch_sparc_t.h

$(srcdir)/be/sparc/gen_sparc_new_nodes.c.inl $(srcdir)/be/sparc/gen_sparc_new_nodes.h: \
		be/sparc/sparc_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/sparc/sparc_spec.pl $(srcdir)/be/sparc

$(srcdir)/be/sparc/gen_sparc_emitter.c $(srcdir)/be/sparc/gen_sparc_emitter.h: \
		be/sparc/sparc_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/sparc/sparc_spec.pl $(srcdir)/be/sparc

$(srcdir)/be/sparc/gen_sparc_regalloc_if.c $(srcdir)/be/sparc/gen_sparc_regalloc_if.h: \
		be/sparc/sparc_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/sparc/sparc_spec.pl $(srcdir)/be/sparc

# amd64 backend

amd64_sources = \
	be/amd64/amd64_emitter.c \
	be/amd64/amd64_new_nodes.c \
	be/amd64/amd64_transform.c \
	be/amd64/bearch_amd64.c
amd64_built_sources = \
	be/amd64/gen_amd64_emitter.c \
	be/amd64/gen_amd64_emitter.h \
	be/amd64/gen_amd64_new_nodes.c.inl \
	be/amd64/gen_amd64_new_nodes.h \
	be/amd64/gen_amd64_regalloc_if.c \
	be/amd64/gen_amd64_regalloc_if.h
MAINTAINERCLEANFILES += $(amd64_built_sources)
BUILT_SOURCES += $(amd64_built_sources)
libfirm_la_SOURCES += $(amd64_sources) $(amd64_built_sources)

EXTRA_DIST += \
	be/amd64/amd64_emitter.h \
	be/amd64/amd64_new_nodes.h \
	be/amd64/amd64_nodes_attr.h \
	be/amd64/amd64_spec.pl \
	be/amd64/amd64_transform.h \
	be/amd64/bearch_amd64_t.h

$(srcdir)/be/amd64/gen_amd64_new_nodes.c.inl $(srcdir)/be/amd64/gen_amd64_new_nodes.h: \
		be/amd64/amd64_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/amd64/amd64_spec.pl $(srcdir)/be/amd64

$(srcdir)/be/amd64/gen_amd64_emitter.c $(srcdir)/be/amd64/gen_amd64_emitter.h: \
		be/amd64/amd64_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/amd64/amd64_spec.pl $(srcdir)/be/amd64

$(srcdir)/be/amd64/gen_amd64_regalloc_if.c $(srcdir)/be/amd64/gen_amd64_regalloc_if.h: \
		be/amd64/amd64_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/amd64/amd64_spec.pl $(srcdir)/be/amd64


# TEMPLATE backend

TEMPLATE_sources = \
	be/TEMPLATE/TEMPLATE_emitter.c \
	be/TEMPLATE/TEMPLATE_new_nodes.c \
	be/TEMPLATE/TEMPLATE_transform.c \
	be/TEMPLATE/bearch_TEMPLATE.c
TEMPLATE_built_sources = \
	be/TEMPLATE/gen_TEMPLATE_emitter.c \
	be/TEMPLATE/gen_TEMPLATE_emitter.h \
	be/TEMPLATE/gen_TEMPLATE_new_nodes.c.inl \
	be/TEMPLATE/gen_TEMPLATE_new_nodes.h \
	be/TEMPLATE/gen_TEMPLATE_regalloc_if.c \
	be/TEMPLATE/gen_TEMPLATE_regalloc_if.h
MAINTAINERCLEANFILES += $(TEMPLATE_built_sources)
BUILT_SOURCES += $(TEMPLATE_built_sources)
libfirm_la_SOURCES += $(TEMPLATE_sources) $(TEMPLATE_built_sources)

EXTRA_DIST += \
	be/TEMPLATE/TEMPLATE_emitter.h \
	be/TEMPLATE/TEMPLATE_new_nodes.h \
	be/TEMPLATE/TEMPLATE_nodes_attr.h \
	be/TEMPLATE/TEMPLATE_spec.pl \
	be/TEMPLATE/TEMPLATE_transform.h \
	be/TEMPLATE/bearch_TEMPLATE_t.h

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_new_nodes.c.inl $(srcdir)/be/TEMPLATE/gen_TEMPLATE_new_nodes.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_emitter.c $(srcdir)/be/TEMPLATE/gen_TEMPLATE_emitter.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_regalloc_if.c $(srcdir)/be/TEMPLATE/gen_TEMPLATE_regalloc_if.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE
