/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Define the famous infame FOURCC macro.
 * @date        02.01.2004
 */
#ifndef FIRM_ADT_FOURCC_H
#define FIRM_ADT_FOURCC_H

/** define a readable fourcc code */
#define FOURCC(a,b,c,d)         ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))

#endif
