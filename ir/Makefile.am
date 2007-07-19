lib_LTLIBRARIES = libfirm.la

INCLUDES = $(LIBCORE_CFLAGS) \
	-I$(top_srcdir)/include/libfirm \
	-I$(top_srcdir)/include/libfirm/adt \
	-I$(top_builddir)

INCLUDES += \
	-I$(top_srcdir)/ir/adt \
	-I$(top_srcdir)/ir/ana \
	-I$(top_srcdir)/ir/arch \
	-I$(top_srcdir)/ir/common \
	-I$(top_srcdir)/ir/config \
	-I$(top_srcdir)/ir/debug \
	-I$(top_srcdir)/ir/ident \
	-I$(top_srcdir)/ir/ir \
	-I$(top_srcdir)/ir/lower \
	-I$(top_srcdir)/ir/net \
	-I$(top_srcdir)/ir/opt \
	-I$(top_srcdir)/ir/stat \
	-I$(top_srcdir)/ir/tr \
	-I$(top_srcdir)/ir/tv
libfirm_la_SOURCES = \
	adt/array.c \
	adt/arrayset.c \
	adt/bipartite.c \
	adt/cpset.c \
	adt/eset.c \
	adt/gaussjordan.c \
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
	ana/compute_loop_info.c \
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
	ana/irlivechk_old.c \
	ana/irmemory.c \
	ana/irouts.c \
	ana/irscc.c \
	ana/irsimpletype.c \
	ana/irtypeinfo.c \
	ana/phiclass.c \
	ana/rta.c \
	ana/structure.c \
	ana/trouts.c \
	arch/archop.c \
	be/beabi.c \
	be/bearch.c \
	be/beblocksched.c \
	be/bechordal.c \
	be/bechordal_draw.c \
	be/bechordal_main.c \
	be/becopyheur2.c \
	be/becopyheur3.c \
	be/becopyheur4.c \
	be/becopyheur.c \
	be/becopyilp1.c \
	be/becopyilp2.c \
	be/becopyilp.c \
	be/becopyopt.c \
	be/becopystat.c \
	be/bedomfront.c \
	be/beemitter.c \
	be/begnuas.c \
	be/beifg.c \
	be/beifg_clique.c \
	be/beifg_list.c \
	be/beifg_pointer.c \
	be/beifg_std.c \
	be/beilpsched.c \
	be/beinsn.c \
	be/beirg.c \
	be/beirgmod.c \
	be/bejavacoal.c \
	be/belistsched.c \
	be/belive.c \
	be/beloopana.c \
	be/belower.c \
	be/bemachine.c \
	be/bemachnode.c \
	be/bemain.c \
	be/bemodule.c \
	be/benode.c \
	be/benodesets.c \
	be/bepressurestat.c \
	be/beprofile.c \
	be/bera.c \
	be/beraextern.c \
	be/besched.c \
	be/beschedmris.c \
	be/beschedrand.c \
	be/beschedregpress.c \
	be/beschedrss.c \
	be/beschedtrace.c \
	be/beschedtrivial.c \
	be/bespillbelady.c \
	be/bespill.c \
	be/bespilldaemel.c \
	be/bespillmorgan.c \
	be/bespilloptions.c \
	be/bespillremat.c \
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
	debug/dbginfo.c \
	debug/debugger.c \
	debug/firm_ycomp.c \
	debug/seqnumbers.c \
	ident/ident.c \
	ident/mangle.c \
	ir/irarch.c \
	ir/irargs.c \
	ir/ircgcons.c \
	ir/ircgopt.c \
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
	ir/irmode.c \
	ir/irnode.c \
	ir/irnodemap.c \
	ir/irnodeset.c \
	ir/irop.c \
	ir/iropt.c \
	ir/irphase.c \
	ir/irprintf.c \
	ir/irprog.c \
	ir/irvrfy.c \
	ir/pseudo_irg.c \
	lower/lower_calls.c \
	lower/lower_dw.c \
	lower/lower_hl.c \
	lower/lower_intrinsics.c \
	lower/lower_mode_b.c \
	net/firmnet.c \
	opt/cfopt.c \
	opt/condeval.c \
	opt/convopt.c \
	opt/data_flow_scalar_replace.c \
	opt/escape_ana.c \
	opt/funccall.c \
	opt/gvn_pre.c \
	opt/ifconv.c \
	opt/ldst2.c \
	opt/ldstopt.c \
	opt/loop_unrolling.c \
	opt/opt_confirms.c \
	opt/opt_frame.c \
	opt/opt_osr.c \
	opt/opt_polymorphy.c \
	opt/proc_cloning.c \
	opt/reassoc.c \
	opt/return.c \
	opt/scalar_replace.c \
	opt/strength_red.c \
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
	tr/entity.c \
	tr/tpop.c \
	tr/tr_inheritance.c \
	tr/trvrfy.c \
	tr/type.c \
	tr/typegmod.c \
	tr/type_identify.c \
	tr/typewalk.c \
	tv/fltcalc.c \
	tv/strcalc.c \
	tv/tv.c

# ia32 backend

libfirm_la_SOURCES += \
	be/ia32/bearch_ia32.c \
	be/ia32/gen_ia32_emitter.c \
	be/ia32/gen_ia32_machine.c \
	be/ia32/gen_ia32_regalloc_if.c \
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
		be/ia32/ia32_spec.pl be/scripts/generate_machine.pl
	$(PERL) $(srcdir)/be/scripts/generate_regalloc_if.pl $(srcdir)/be/ia32/ia32_spec.pl $(srcdir)/be/ia32

# arm backend

libfirm_la_SOURCES += \
	be/arm/bearch_arm.c \
	be/arm/gen_arm_emitter.c \
	be/arm/gen_arm_machine.c \
	be/arm/gen_arm_regalloc_if.c \
	be/arm/arm_emitter.c \
	be/arm/arm_map_regs.c \
	be/arm/arm_new_nodes.c \
	be/arm/arm_transform.c

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

libfirm_la_SOURCES += \
	be/mips/bearch_mips.c \
	be/mips/gen_mips_emitter.c \
	be/mips/gen_mips_machine.c \
	be/mips/gen_mips_regalloc_if.c \
	be/mips/mips_emitter.c \
	be/mips/mips_map_regs.c \
	be/mips/mips_new_nodes.c \
	be/mips/mips_scheduler.c \
	be/mips/mips_transform.c

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

libfirm_la_SOURCES += \
	be/ppc32/bearch_ppc32.c \
	be/ppc32/gen_ppc32_emitter.c \
	be/ppc32/gen_ppc32_machine.c \
	be/ppc32/gen_ppc32_regalloc_if.c \
	be/ppc32/ppc32_emitter.c \
	be/ppc32/ppc32_map_regs.c \
	be/ppc32/ppc32_new_nodes.c \
	be/ppc32/ppc32_transform.c \
	be/ppc32/ppc32_transform_conv.c

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
