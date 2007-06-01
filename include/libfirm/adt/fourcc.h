/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */

/**
 * @file
 * @brief       Define the famous infame FOURCC macro.
 * @date        02.01.2004
 * @version     $Id$
 */
#ifndef FIRM_ADT_FOURCC_H
#define FIRM_ADT_FOURCC_H

/** define a readable fourcc code */
#define FOURCC(a,b,c,d)         ((a) | ((b) << 8) | ((c) << 16) | ((d) << 24))

#endif
