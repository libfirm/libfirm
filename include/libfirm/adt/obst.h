/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief     Provides obstack_chunk_alloc and obstack_chunk_free for obstack.h
 * @author    Martin Trapp, Christian Schaefer
 */
#ifndef FIRM_ADT_OBST_H
#define FIRM_ADT_OBST_H

#include "obstack.h"
#include "xmalloc.h"

/** @cond PRIVATE */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free  free
/** @endcond */

#endif
