/**
 * @file   irphases_t.h
 * @date   18.11.2007
 * @author Sebastian Hack
 *
 * Copyright (C) 2007 Inria Rhone-Alpes
 * Released under the GPL
 */

#ifndef _IRPHASES_T_H
#define _IRPHASES_T_H

enum _ir_phase_id {
#define PH(name, description)	PHASE_ ## name,
#include "irphaselist.h"
#undef PH
};

typedef enum _ir_phase_id ir_phase_id;

#endif /* _IRPHASES_T_H */
