lib_LTLIBRARIES = libfirm.la

INCLUDES = $(LIBCORE_CFLAGS) \
	-I$(top_srcdir)/include/libfirm \
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
	-I$(top_srcdir)/ir/libcore \
	-I$(top_srcdir)/ir/lower \
	-I$(top_srcdir)/ir/net \
	-I$(top_srcdir)/ir/opt \
	-I$(top_srcdir)/ir/stat \
	-I$(top_srcdir)/ir/tr \
	-I$(top_srcdir)/ir/tv

libfirm_la_SOURCES = \
	adt/array.c \
	adt/bipartite.c \
	adt/cpset.c \
	adt/eset.c \
	adt/gaussjordan.c \
	adt/gaussseidel.c \
	adt/hashset.c \
	adt/hungarian.c \
	adt/iterator.c \
	adt/pdeq.c \
	adt/plist.c \
	adt/pmap.c \
	adt/pqueue.c \
	adt/pset.c \
	adt/pset_new.c \
	adt/set.c \
	adt/xmalloc.c \
	ana/absgraph.c \
	ana/analyze_irg_args.c \
	ana/callgraph.c \
	ana/cdep.c \
	ana/cgana.c \
	ana/dfs.c \
	ana/execfreq.c \
	ana/execution_frequency.c \
	ana/field_temperature.c \
	ana/height.c \
	ana/interval_analysis.c \
	ana/irbackedge.c \
	ana/ircfscc.c \
	ana/irconsconfirm.c \
	ana/irdom.c \
	ana/irextbb2.c \
	ana/irextbb.c \
	ana/irlivechk.c \
	ana/irloop.c \
	ana/irmemory.c \
	ana/irouts.c \
	ana/irscc.c \
	ana/irsimpletype.c \
	ana/irtypeinfo.c \
	ana/phiclass.c \
	ana/rta.c \
	ana/structure.c \
	ana/trouts.c \
	be/beabi.c \
	be/bearch.c \
	be/beblocksched.c \
	be/bechordal.c \
	be/bechordal_draw.c \
	be/bechordal_main.c \
	be/becopyheur2.c \
	be/becopyheur4.c \
	be/becopyheur.c \
	be/becopyilp1.c \
	be/becopyilp2.c \
	be/becopyilp.c \
	be/becopyopt.c \
	be/becopystat.c \
	be/bedbgout.c \
	be/bedomfront.c \
	be/beemitter.c \
	be/beflags.c \
	be/begnuas.c \
	be/beifg.c \
	be/beifg_clique.c \
	be/beifg_list.c \
	be/beifg_pointer.c \
	be/beifg_std.c \
	be/beilpsched.c \
	be/beinfo.c \
	be/beinsn.c \
	be/beirg.c \
	be/beirgmod.c \
	be/belistsched.c \
	be/belive.c \
	be/beloopana.c \
	be/belower.c \
	be/bemachine.c \
	be/bemachnode.c \
	be/bemain.c \
	be/bemodule.c \
	be/benode.c \
	be/bepeephole.c \
	be/bepressurestat.c \
	be/bera.c \
	be/besched.c \
	be/beschedmris.c \
	be/beschednormal.c \
	be/beschedrand.c \
	be/beschedregpress.c \
	be/beschedrss.c \
	be/beschedtrace.c \
	be/beschedtrivial.c \
	be/bespillbelady2.c \
	be/bespillbelady3.c \
	be/bespillbelady.c \
	be/bespill.c \
	be/bespilldaemel.c \
	be/bespilloptions.c \
	be/bespillslots.c \
	be/bessaconstr.c \
	be/bessadestr.c \
	be/bessadestrsimple.c \
	be/bestabs.c \
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
	common/firmwalk.c \
	common/irtools.c \
	common/statistics.c \
	common/timing.c \
	debug/dbginfo.c \
	debug/debugger.c \
	debug/firm_ycomp.c \
	debug/seqnumbers.c \
	ident/ident.c \
	ident/mangle.c \
	ir/instrument.c \
	ir/irarch.c \
	ir/irargs.c \
	ir/ircgcons.c \
	ir/ircons.c \
	ir/irdump.c \
	ir/irdump_grgen.c \
	ir/irdumptxt.c \
	ir/iredges.c \
	ir/irflag.c \
	ir/irgmod.c \
	ir/irgopt.c \
	ir/irgraph.c \
	ir/irgwalk_blk.c \
	ir/irgwalk.c \
	ir/irhooks.c \
	ir/irlinkednodemap.c \
	ir/irlinkednodeset.c \
	ir/irmode.c \
	ir/irnode.c \
	ir/irnodemap.c \
	ir/irnodeset.c \
	ir/irop.c \
	ir/iropt.c \
	ir/irphase.c \
	ir/irprintf.c \
	ir/irprofile.c \
	ir/irprog.c \
	ir/irssacons.c \
	ir/irvrfy.c \
	ir/pseudo_irg.c \
	ir/valueset.c \
	libcore/lc_appendable.c \
	libcore/lc_config_lexer.c \
	libcore/lc_config_parser.c \
	libcore/lc_opts.c \
	libcore/lc_opts_enum.c \
	libcore/lc_printf.c \
	libcore/lc_type.c \
	lower/lower_calls.c \
	lower/lower_copyb.c \
	lower/lower_dw.c \
	lower/lower_hl.c \
	lower/lower_intrinsics.c \
	lower/lower_mode_b.c \
	lower/lower_switch.c \
	net/firmnet.c \
	obstack/obstack.c \
	obstack/obstack_printf.c \
	opt/boolopt.c \
	opt/cfopt.c \
	opt/code_placement.c \
	opt/combo.c \
	opt/condeval.c \
	opt/convopt.c \
	opt/critical_edges.c \
	opt/data_flow_scalar_replace.c \
	opt/escape_ana.c \
	opt/funccall.c \
	opt/gvn_pre.c \
	opt/ifconv.c \
	opt/ircgopt.c \
	opt/ldst2.c \
	opt/ldstopt.c \
	opt/opt_blocks.c \
	opt/opt_confirms.c \
	opt/opt_frame.c \
	opt/opt_inline.c \
	opt/opt_osr.c \
	opt/opt_polymorphy.c \
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
	stat/stat_liveness.c \
	stat/stat_timing.c \
	tr/entity.c \
	tr/tpop.c \
	tr/tr_inheritance.c \
	tr/trvrfy.c \
	tr/type.c \
	tr/type_finalization.c \
	tr/typegmod.c \
	tr/type_identify.c \
	tr/typewalk.c \
	tv/fltcalc.c \
	tv/strcalc.c \
	tv/tv.c

EXTRA_DIST = \
	adt/bitset_std.h \
	adt/array_t.h \
	adt/bitset.h \
	adt/compiler.h \
	adt/bitset_ia32.h \
	adt/bitfiddle.h \
	ana/irdom_t.h \
	ana/irloop_t.h \
	ana/irbackedge_t.h \
	ana/dfs_t.h \
	ana/irmemory_t.h \
	ana/irextbb_t.h \
	be/belive_t.h \
	be/beifg_impl.h \
	be/bessaconstr.h \
	be/becopyopt_t.h \
	be/beifg_t.h \
	be/beblocksched.h \
	be/bessadestrsimple.h \
	be/becopyilp_t.h \
	be/bemachine.h \
	be/beverify.h \
	be/beemitter.h \
	be/begnuas.h \
	be/bemodule.h \
	be/be_t.h \
	be/belistsched.h \
	be/bessadestr.h \
	be/bechordal_draw.h \
	be/beabi.h \
	be/bechordal_t.h \
	be/besched_t.h \
	be/beifg.h \
	be/bespillslots.h \
	be/besched.h \
	be/beschedmris.h \
	be/beilpsched.h \
	be/beloopana.h \
	be/beuses.h \
	be/becopyopt.h \
	be/bepressurestat.h \
	be/beintlive_t.h \
	be/bestat.h \
	be/bemachnode.h \
	be/beabi_t.h \
	be/bespill.h \
	be/bestate.h \
	be/bespilloptions.h \
	be/bearch_t.h \
	be/bera.h \
	be/beirg_t.h \
	be/beirgmod.h \
	be/beinsn_t.h \
	be/belive.h \
	be/bearch.h \
	be/beutil.h \
	be/bepeephole.h \
	be/bestatevent.h \
	be/be_dbgout.h \
	be/benode_t.h \
	be/belower.h \
	be/bedomfront.h \
	be/beinfo.h \
	be/becopystat.h \
	be/bemodule_t.h \
	be/betranshlp.h \
	be/beflags.h \
	be/bechordal.h \
	be/beschedrss.h \
	be/beirg.h \
	be/be_dbgout_t.h \
	common/error.h \
	common/statistics.h \
	common/irtools.h \
	common/debug.h \
	common/firmwalk.h \
	common/firm_common_t.h \
	debug/dbginfo_t.h \
	debug/debugger.h \
	ident/ident_t.h \
	ir/irmode_t.h \
	ir/irprog_t.h \
	ir/irvrfy_t.h \
	ir/iredges_t.h \
	ir/irtypes.h \
	ir/ircons_t.h \
	ir/irgraph_t.h \
	ir/irnodeset.h \
	ir/irflag_t.def \
	ir/irop_t.h \
	ir/irnode_t.h \
	ir/irflag_t.h \
	ir/irargs_t.h \
	ir/irdump_t.h \
	ir/irbitset.h \
	ir/irnodemap.h \
	ir/irprofile.h \
	ir/irprintf_t.h \
	ir/irphase_t.h \
	ir/iropt_t.h \
	ir/irphases_t.h \
	ir/irphaselist.h \
	ir/instrument.h \
	ir/iredgeset.h \
	ir/valueset.h \
	ir/irlinkednodemap.h \
	ir/irlinkednodeset.h \
	ir/irdump_grgen.h \
	libcore/lc_common_t.h \
	libcore/lc_defines.h \
	libcore/lc_parser_t.h \
	libcore/do_bisonflex.sh \
	libcore/lc_config_parser.h \
	libcore/lc_opts.h \
	libcore/lc_printf_arg_types.def \
	libcore/lc_appendable.h \
	libcore/lc_printf.h \
	libcore/lc_config.h \
	libcore/lc_opts_t.h \
	libcore/lc_opts_enum.h \
	net/firmnet.h \
	net/firmnet_t.h \
	opt/scalar_replace.h \
	opt/reassoc_t.h \
	opt/opt_polymorphy.h \
	opt/opt_confirms.h \
	opt/tropt.h \
	opt/funccall_t.h \
	opt/opt_inline_t.h \
	stat/stat_dmp.h \
	stat/counter.h \
	stat/firmstat_t.h \
	stat/pattern.h \
	stat/pattern_dmp.h \
	stat/dags.h \
	stat/statev.h \
	stat/stat_timing.h \
	tr/entity_t.h \
	tr/type_t.h \
	tr/tpop_t.h \
	tv/strcalc.h \
	tv/tv_t.h \
	tv/fltcalc.h \
	be/scripts/generate_emitter_new.pl \
	be/scripts/generate_emitter.pl \
	be/scripts/generate_machine.pl \
	be/scripts/generate_new_opcodes.pl \
	be/scripts/generate_regalloc_if.pl

EXTRA_DIST += libcore/do_bisonflex.sh \
	libcore/lc_config_parser.y \
	libcore/lc_config_lexer.l

# ia32 backend

ia32_sources = \
	be/ia32/bearch_ia32.c \
	be/ia32/ia32_architecture.c \
	be/ia32/ia32_address_mode.c \
	be/ia32/ia32_common_transform.c \
	be/ia32/ia32_emitter.c \
	be/ia32/ia32_finish.c \
	be/ia32/ia32_fpu.c \
	be/ia32/ia32_intrinsics.c \
	be/ia32/ia32_map_regs.c \
	be/ia32/ia32_new_nodes.c \
	be/ia32/ia32_optimize.c \
	be/ia32/ia32_transform.c \
	be/ia32/ia32_util.c \
	be/ia32/ia32_x87.c

ia32_built_sources = \
	be/ia32/gen_ia32_emitter.c \
	be/ia32/gen_ia32_emitter.h \
	be/ia32/gen_ia32_machine.c \
	be/ia32/gen_ia32_machine.h \
	be/ia32/gen_ia32_regalloc_if.c \
	be/ia32/gen_ia32_regalloc_if.h \
	be/ia32/gen_ia32_new_nodes.c.inl \
	be/ia32/gen_ia32_new_nodes.h
MAINTAINERCLEANFILES = $(ia32_built_sources)
BUILT_SOURCES        = $(ia32_built_sources)
libfirm_la_SOURCES  += $(ia32_sources) $(ia32_built_sources)

EXTRA_DIST += \
	be/ia32/ia32_spec.pl \
	be/ia32/ia32_simd_spec.pl \
	be/ia32/bearch_ia32_t.h \
	be/ia32/ia32_architecture.h \
	be/ia32/ia32_address_mode.h \
	be/ia32/ia32_common_transform.h \
	be/ia32/ia32_dbg_stat.h \
	be/ia32/ia32_emitter.h \
	be/ia32/ia32_finish.h \
	be/ia32/ia32_fpu.h \
	be/ia32/ia32_map_regs.h \
	be/ia32/ia32_new_nodes.h \
	be/ia32/ia32_nodes_attr.h \
	be/ia32/ia32_optimize.h \
	be/ia32/ia32_transform.h \
	be/ia32/ia32_util.h \
	be/ia32/ia32_x87.h

$(srcdir)/be/ia32/gen_ia32_new_nodes.c.inl $(srcdir)/be/ia32/gen_ia32_new_nodes.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

$(srcdir)/be/ia32/gen_ia32_emitter.c $(srcdir)/be/ia32/gen_ia32_emitter.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

$(srcdir)/be/ia32/gen_ia32_machine.c $(srcdir)/be/ia32/gen_ia32_machine.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_machine.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

$(srcdir)/be/ia32/gen_ia32_regalloc_if.c $(srcdir)/be/ia32/gen_ia32_regalloc_if.h: \
		be/ia32/ia32_spec.pl be/scripts/generate_regalloc_if.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

# arm backend

arm_sources = \
	be/arm/bearch_arm.c \
	be/arm/arm_emitter.c \
	be/arm/arm_map_regs.c \
	be/arm/arm_new_nodes.c \
	be/arm/arm_optimize.c \
	be/arm/arm_transform.c
arm_built_sources = \
	be/arm/gen_arm_emitter.c \
	be/arm/gen_arm_emitter.h \
	be/arm/gen_arm_machine.c \
	be/arm/gen_arm_machine.h \
	be/arm/gen_arm_regalloc_if.c \
	be/arm/gen_arm_regalloc_if.h \
	be/arm/gen_arm_new_nodes.c.inl \
	be/arm/gen_arm_new_nodes.h
MAINTAINERCLEANFILES += $(arm_built_sources)
BUILT_SOURCES += $(arm_built_sources)
libfirm_la_SOURCES += $(arm_sources) $(arm_built_sources)


EXTRA_DIST += \
	be/arm/arm_spec.pl \
	be/arm/arm_emitter.h \
	be/arm/arm_map_regs.h \
	be/arm/arm_new_nodes.h \
	be/arm/arm_nodes_attr.h \
	be/arm/arm_optimize.h \
	be/arm/arm_transform.h \
	be/arm/bearch_arm.h \
	be/arm/bearch_arm_t.h

$(srcdir)/be/arm/gen_arm_new_nodes.c.inl $(srcdir)/be/arm/gen_arm_new_nodes.h: \
		be/arm/arm_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

$(srcdir)/be/arm/gen_arm_emitter.c $(srcdir)/be/arm/gen_arm_emitter.h: \
		be/arm/arm_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

$(srcdir)/be/arm/gen_arm_machine.c $(srcdir)/be/arm/gen_arm_machine.h: \
		be/arm/arm_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_machine.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

$(srcdir)/be/arm/gen_arm_regalloc_if.c $(srcdir)/be/arm/gen_arm_regalloc_if.h: \
		be/arm/arm_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/arm/arm_spec.pl $(srcdir)/be/arm

# mips backend

mips_sources = \
	be/mips/bearch_mips.c \
	be/mips/mips_emitter.c \
	be/mips/mips_map_regs.c \
	be/mips/mips_new_nodes.c \
	be/mips/mips_scheduler.c \
	be/mips/mips_transform.c
mips_built_sources = \
	be/mips/gen_mips_emitter.c \
	be/mips/gen_mips_emitter.h \
	be/mips/gen_mips_machine.c \
	be/mips/gen_mips_machine.h \
	be/mips/gen_mips_regalloc_if.c \
	be/mips/gen_mips_regalloc_if.h \
	be/mips/gen_mips_new_nodes.c.inl \
	be/mips/gen_mips_new_nodes.h
MAINTAINERCLEANFILES += $(mips_built_sources)
BUILT_SOURCES += $(mips_built_sources)
libfirm_la_SOURCES += $(mips_sources) $(mips_built_sources)

EXTRA_DIST += \
	be/mips/mips_spec.pl \
	be/mips/bearch_mips.h \
	be/mips/bearch_mips_t.h \
	be/mips/mips_emitter.h \
	be/mips/mips_map_regs.h \
	be/mips/mips_new_nodes.h \
	be/mips/mips_nodes_attr.h \
	be/mips/mips_scheduler.h \
	be/mips/mips_transform.h \
	be/mips/mips_util.h

$(srcdir)/be/mips/gen_mips_new_nodes.c.inl $(srcdir)/be/mips/gen_mips_new_nodes.h: \
		be/mips/mips_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/mips/mips_spec.pl $(srcdir)/be/mips

$(srcdir)/be/mips/gen_mips_emitter.c $(srcdir)/be/mips/gen_mips_emitter.h: \
		be/mips/mips_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/mips/mips_spec.pl $(srcdir)/be/mips

$(srcdir)/be/mips/gen_mips_machine.c $(srcdir)/be/mips/gen_mips_machine.h: \
		be/mips/mips_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_machine.pl $(srcdir)/be/mips/mips_spec.pl $(srcdir)/be/mips

$(srcdir)/be/mips/gen_mips_regalloc_if.c $(srcdir)/be/mips/gen_mips_regalloc_if.h: \
		be/mips/mips_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/mips/mips_spec.pl $(srcdir)/be/mips

# ppc32 backend

ppc32_sources = \
	be/ppc32/bearch_ppc32.c \
	be/ppc32/ppc32_emitter.c \
	be/ppc32/ppc32_map_regs.c \
	be/ppc32/ppc32_new_nodes.c \
	be/ppc32/ppc32_transform.c \
	be/ppc32/ppc32_transform_conv.c
ppc32_built_sources = \
	be/ppc32/gen_ppc32_emitter.c \
	be/ppc32/gen_ppc32_emitter.h \
	be/ppc32/gen_ppc32_machine.c \
	be/ppc32/gen_ppc32_machine.h \
	be/ppc32/gen_ppc32_regalloc_if.c \
	be/ppc32/gen_ppc32_regalloc_if.h \
	be/ppc32/gen_ppc32_new_nodes.c.inl \
	be/ppc32/gen_ppc32_new_nodes.h
MAINTAINERCLEANFILES += $(ppc32_built_sources)
BUILT_SOURCES += $(ppc32_built_sources)
libfirm_la_SOURCES += $(ppc32_sources) $(ppc32_built_sources)

EXTRA_DIST += \
	be/ppc32/ppc32_spec.pl \
	be/ppc32/bearch_ppc32.h \
	be/ppc32/bearch_ppc32_t.h \
	be/ppc32/ppc32_emitter.h \
	be/ppc32/ppc32_map_regs.h \
	be/ppc32/ppc32_new_nodes.h \
	be/ppc32/ppc32_nodes_attr.h \
	be/ppc32/ppc32_transform_conv.h \
	be/ppc32/ppc32_transform.h

$(srcdir)/be/ppc32/gen_ppc32_new_nodes.c.inl $(srcdir)/be/ppc32/gen_ppc32_new_nodes.h: \
		be/ppc32/ppc32_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/ppc32/ppc32_spec.pl $(srcdir)/be/ppc32

$(srcdir)/be/ppc32/gen_ppc32_emitter.c $(srcdir)/be/ppc32/gen_ppc32_emitter.h: \
		be/ppc32/ppc32_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/ppc32/ppc32_spec.pl $(srcdir)/be/ppc32

$(srcdir)/be/ppc32/gen_ppc32_machine.c $(srcdir)/be/ppc32/gen_ppc32_machine.h: \
		be/ppc32/ppc32_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_machine.pl $(srcdir)/be/ppc32/ppc32_spec.pl $(srcdir)/be/ppc32

$(srcdir)/be/ppc32/gen_ppc32_regalloc_if.c $(srcdir)/be/ppc32/gen_ppc32_regalloc_if.h: \
		be/ppc32/ppc32_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/ppc32/ppc32_spec.pl $(srcdir)/be/ppc32

# TEMPLATE backend

TEMPLATE_sources = \
	be/TEMPLATE/bearch_TEMPLATE.c \
	be/TEMPLATE/TEMPLATE_emitter.c \
	be/TEMPLATE/TEMPLATE_map_regs.c \
	be/TEMPLATE/TEMPLATE_new_nodes.c \
	be/TEMPLATE/TEMPLATE_transform.c
TEMPLATE_built_sources = \
	be/TEMPLATE/gen_TEMPLATE_emitter.c \
	be/TEMPLATE/gen_TEMPLATE_emitter.h \
	be/TEMPLATE/gen_TEMPLATE_machine.c \
	be/TEMPLATE/gen_TEMPLATE_machine.h \
	be/TEMPLATE/gen_TEMPLATE_regalloc_if.c \
	be/TEMPLATE/gen_TEMPLATE_regalloc_if.h \
	be/TEMPLATE/gen_TEMPLATE_new_nodes.c.inl \
	be/TEMPLATE/gen_TEMPLATE_new_nodes.h
MAINTAINERCLEANFILES += $(TEMPLATE_built_sources)
BUILT_SOURCES += $(TEMPLATE_built_sources)
libfirm_la_SOURCES += $(TEMPLATE_sources) $(TEMPLATE_built_sources)

EXTRA_DIST += \
	be/TEMPLATE/TEMPLATE_spec.pl \
	be/TEMPLATE/bearch_TEMPLATE.h \
	be/TEMPLATE/bearch_TEMPLATE_t.h \
	be/TEMPLATE/TEMPLATE_emitter.h \
	be/TEMPLATE/TEMPLATE_map_regs.h \
	be/TEMPLATE/TEMPLATE_new_nodes.h \
	be/TEMPLATE/TEMPLATE_nodes_attr.h \
	be/TEMPLATE/TEMPLATE_transform.h

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_new_nodes.c.inl $(srcdir)/be/TEMPLATE/gen_TEMPLATE_new_nodes.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_new_opcodes.pl
	$(PERL) $(srcdir)/be/scripts/generate_new_opcodes.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_emitter.c $(srcdir)/be/TEMPLATE/gen_TEMPLATE_emitter.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_emitter.pl
	$(PERL) $(srcdir)/be/scripts/generate_emitter.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_machine.c $(srcdir)/be/TEMPLATE/gen_TEMPLATE_machine.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_machine.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE

$(srcdir)/be/TEMPLATE/gen_TEMPLATE_regalloc_if.c $(srcdir)/be/TEMPLATE/gen_TEMPLATE_regalloc_if.h: \
		be/TEMPLATE/TEMPLATE_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/TEMPLATE/TEMPLATE_spec.pl $(srcdir)/be/TEMPLATE
