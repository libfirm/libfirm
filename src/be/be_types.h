/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Forward declarations of backend types
 * @author      Matthias Braun
 */
#ifndef FIRM_BE_TYPES_H
#define FIRM_BE_TYPES_H

#include <stdint.h>

#include "firm_types.h"

typedef unsigned int sched_timestep_t;

typedef struct arch_register_class_t     arch_register_class_t;
typedef struct arch_register_req_t       arch_register_req_t;
typedef struct arch_register_t           arch_register_t;
typedef struct arch_isa_if_t             arch_isa_if_t;

/**
 * Some flags describing a node in more detail.
 */
typedef enum arch_irn_flags_t {
	arch_irn_flags_none            = 0,       /**< No flags. */
	arch_irn_flag_dont_spill       = 1U << 0, /**< This must not be spilled. */
	arch_irn_flag_rematerializable = 1U << 1, /**< This can be replicated instead of spilled/reloaded. */
	arch_irn_flag_modify_flags     = 1U << 2, /**< I modify flags, used by the
	                                               default check_modifies
	                                               implementation in beflags */
	arch_irn_flag_simple_jump      = 1U << 3, /**< a simple jump instruction */
	arch_irn_flag_not_scheduled    = 1U << 4, /**< node must not be scheduled*/
	/** node is scheduled as soon as possible. Note that the schedule
	 * algorithms cannot decide the order if multiple schedule_first nodes are
	 * ready. */
	arch_irn_flag_schedule_first   = 1U << 5,
	/** node writes to a spillslot, this means we can load from the spillslot
	 * anytime (important when deciding whether we can rematerialize) */
	arch_irn_flag_spill            = 1U << 6,
	/** node performs a reload like operation */
	arch_irn_flag_reload           = 1U << 7,
	arch_irn_flag_fallthrough      = 1U << 8,
	/** begin of custom backend flags */
	arch_irn_flag_backend          = 1U << 9,
} arch_irn_flags_t;
ENUM_BITSET(arch_irn_flags_t)

typedef struct be_lv_t         be_lv_t;
typedef struct be_lv_info_t    be_lv_info_t;
typedef struct backend_info_t  backend_info_t;
typedef struct backend_params  backend_params;
typedef struct sched_info_t    sched_info_t;
typedef struct reg_out_info_t  reg_out_info_t;
typedef struct be_ifg_t        be_ifg_t;
typedef struct copy_opt_t      copy_opt_t;
typedef struct be_main_env_t   be_main_env_t;
typedef struct be_options_t    be_options_t;
typedef struct regalloc_if_t   regalloc_if_t;

typedef struct be_register_name_t be_register_name_t;

/** Additional register pressure applied to before (positive value) or after
 * (negative value) a instruction. */
typedef int8_t be_add_pressure_t;

#endif
