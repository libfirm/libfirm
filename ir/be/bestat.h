#ifndef _BESTAT_H_
#define _BESTAT_H_

#include "be_t.h"

/**
 * Collects statistics information about register pressure.
 * @param birg The be irg object containing the irg
 */
void be_do_stat_reg_pressure(be_irg_t *birg);

#endif /* _BESTAT_H_ */
