/**
 * @author Daniel Grund
 * @date 11.04.2005
 */

//TODO
#ifndef _BECOPYSTAT_H
#define _BECOPYSTAT_H

#include "becopyopt.h"

typedef struct _irg_stat_t irg_stat_t;

irg_stat_t *new_irg_stat(copy_opt_t *co);
void irg_stat_count(irg_stat_t *is, copy_opt_t *co, int phase);
void irg_stat_print(irg_stat_t *is);
void all_stat_dump(void);

#endif
