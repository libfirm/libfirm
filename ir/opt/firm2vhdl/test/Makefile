-include config.mak

CPARSER ?= cparser
GHDL ?= ghdl
GHDLCFLAGS ?= -g
CFLAGS ?= -m32 -Wall -Wno-unused-parameter

help:
	@echo interesting targets:
	@echo
	@echo "    vhdl - generate .vhd files from testcases"
	@echo "patterns - generate std.textio compatible pattern files"
	@echo "    ghdl - run testbench w/ GHDL"
	@echo "    vsim - run testbench w/ ModelSim"
	@echo "     ngc - synthesize testcases w/ Xilinx xst"
	@echo
	@echo "To run individual tests, use targets t_foo.ghdl, t_foo.vsim, etc."

# Compile test cases and drivers
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Link test case w/ test driver
t_%.driver: t_%.o driver_manual.o
	$(CC) $(CFLAGS) $+ -o $@

# Generate std.textio compatible pattern files from above
t_%.in t_%.expect: t_%.driver
	./$< $(basename $<).in $(basename $<).expect

# Generate expected patterns for all testcases
patterns: $(addsuffix .expect,$(basename $(wildcard t_*.c)))


# Dump IRG of a test case
t_%.ir: t_%.c
	$(CPARSER) --export-ir $< -o $@

# Make sure ir2vhdl is current
../ir2vhdl: ../*.c ../*.h
	cd .. && $(MAKE) ir2vhdl

# Generate VHDL from IR
%.vhd: %.ir ../ir2vhdl
	../ir2vhdl $< $@

# Generate VHDL files for all testcases
vhdl: $(addsuffix .vhd,$(basename $(wildcard t_*.c)))



GHDLWORKDIRPREFIX = ghdlwork_
GHDLWD=$(GHDLWORKDIRPREFIX)$<

# Simulate a testcase using ghdl
t_%.ghdl: t_%.vhd test_atom.vhd testbench.vhd t_%.in t_%.expect
	[ -d $(GHDLWD) ] || mkdir $(GHDLWD)
	cd $(GHDLWD) && ghdl -a $(GHDLCFLAGS) ../test_atom.vhd ../$< ../testbench.vhd
	cd $(GHDLWD) && ghdl -e  $(GHDLCFLAGS) -o ../$@ testbench
	./$@ --ieee-asserts=disable --vcd=$@.vcd < $(basename $<).in > $(basename $<).out
	diff $(basename $<).expect  $(basename $<).out

# Default to testing with ghdl
test: ghdl

# Simulate all testcases using ghdl
ghdl: $(addsuffix .ghdl,$(basename $(wildcard t_*.c)))



XST ?= xst

# XILINX_PART=xc3s100e-4-vq100
XILINX_PART ?= xc5vlx110tff1136-1 # Used at CES

XST_FLAGS ?= -p $(XILINX_PART) -opt_mode Speed -opt_level 1

test_atom.ngc : test_atom.vhd
	-echo "run -ifn $< -ifmt VHDL -ofn $@ $(XST_FLAGS) "|$(XST) > $@.log
	grep -nH ^WARNING $@.log; grep -nH ^ERROR $@.log;:
	touch $@

# Synthesize using Xilinx xst
t_%.ngc t_%.ngc.log : t_%.vhd test_atom.ngc
	echo "run -rtlview yes -ifn $< -ifmt VHDL -ofn $@ $(XST_FLAGS)  "|$(XST)> $@.log
	grep -nH ^WARNING $@.log; grep -nH ^ERROR $@.log;:

# RTL Schematic
t_%.ngr: t_%.ngc

# Native Generic Database - decompose into FPGA native elements
t_%.ngd: t_%.ngc 
	ngdbuild -p $(XILINX_PART) $<

# Generic -> Specific elements and design rules check
t_%.map: t_%.ngd
	map -detail -pr b $<

t_%.edif: t_%.ngc
	ngc2edif $< $@

# par - Place and Route
# bitgen - 
# promgen - 

# Run all testcases through xst
ngc: $(addsuffix .ngc,$(basename $(wildcard t_*.c)))


# ModelSim

INI ?= /Software/ModelSim/ModelSim_6.6d/modeltech/modelsim.ini

work:
	vlib work

work/test_atom: test_atom.vhd work
	vcom -modelsimini $(INI) $<
work/testbench: testbench.vhd work work/test_atom
	vcom -modelsimini $(INI) $<

work/t_%: t_%.vhd work/test_atom
	vcom -modelsimini $(INI) $< > $(basename $<).vcom.log

# Simulate a testcase using Modelsim
t_%.vsim: work/test_atom work/t_% work/testbench t_%.expect t_%.in
	echo 'run -all; quit -sim;' | vsim -modelsimini $(INI) -std_input $(basename $@).in -std_output $(basename $@).out testbench > $@.log
	@grep Warning $@.log;:
	diff $(basename $@).expect  $(basename $@).out

# Simulate all testcases using Modelsim
vsim: $(addsuffix .vsim,$(basename $(wildcard t_*.c)))

# TODO: vcom -check_synthesis target


clean: c-clean firm-clean xilinx-clean vhdl-clean modelsim-clean ghdl-clean

c-clean:
	rm -f *.o  test_atom*.vcg  t_*.ir *.driver

firm-clean:
	rm -f t_*.vcg  t_*.ir 

xilinx-clean:
	rm -f *.ngc *.xrpt *.ngc.log *.mrp *.map *.bld  *_summary.xml *_usage.xml *.pcf *.analyze.log *.ngm *.ngd *.ngr *.edif *.ngr.log ngc2edif.log
	rm -rf xst _xmsgs xlnx_auto_0_xdb

vhdl-clean:
	rm -f t_*.in t_*.out t_*.expect testbench t_*.vhd

modelsim-clean:
	rm -rf work
	rm -f transcript vsim.wlf netlist.lst *.vsim.log *.vcom.log

ghdl-clean:
	rm -f *.cf e~*.o e~*.lst *.ghdl *.ghdl.vcd
	rm -rf $(GHDLWORKDIRPREFIX)*

.PHONY: t_%.test

.PRECIOUS: t_%.in t_%.out %.vhd *.out %.out
.SECONDARY: t_%.out 

