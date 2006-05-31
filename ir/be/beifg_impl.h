/**
 * @file   beifg_impl.h
 * @date   01.12.2005
 * @author Sebastian Hack
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 *
 * Constructors for different implementations of
 * chordal interference graphs.
 */

#ifndef _BEIFG_IMPL_H
#define _BEIFG_IMPL_H

#include "bechordal_t.h"

be_ifg_t *be_ifg_std_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_list_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_clique_new(const be_chordal_env_t *env);
be_ifg_t *be_ifg_pointer_new(const be_chordal_env_t *env);

#endif /* _BEIFG_IMPL_H */
