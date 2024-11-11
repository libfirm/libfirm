/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Lower floating point operations to function calls
 * @author  Sebastian Buchwald
 */
#ifndef FIRM_LOWER_LOWER_SOFTFLOAT_H
#define FIRM_LOWER_LOWER_SOFTFLOAT_H

/**
 * Lowers all floating-point operations.
 *
 * They are replaced by calls into a soft float library.
 */
void lower_floating_point(void);

#endif
