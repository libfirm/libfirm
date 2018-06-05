#ifndef FIRM2VHDL_H
#define FIRM2VHDL_H

#include <stdint.h>
#include "vhdlmodes.h"

/* Register new Modes and Nodes w/ libfirm. */
void init_firm2vhdl(void);
void irg2vhdl(FILE *output, ir_graph *irg);

/* INITIALIZER for internal signals.  Used to avoid Metavalue warnings
   during simulation. TODO: check if it has effects on synthesis. */
#define SIGNAL_INITIALIZER ":= (others => '0')"

#endif
