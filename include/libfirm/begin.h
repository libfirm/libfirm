/*
 * Copyright (C) 1995-2008 University of Karlsruhe.  All right reserved.
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
 * @brief       Should be included in all public firm headers at the beginning
 * @author      Matthias Braun
 * @version     $Id$
 */
#ifndef FIRM_BEGIN_H
#define FIRM_BEGIN_H

/**
 * @def FIRM_API
 * Declaration specifier which marks a function/variable as being publicly
 * visible outside the firm library/dll
 */
#ifdef FIRM_DLL
	#ifdef FIRM_BUILD
		#ifdef _WIN32
			#define FIRM_API extern __declspec(dllexport)
		#else
			#define FIRM_API extern __attribute__((visibility("default")))
		#endif
	#else
		#ifdef _WIN32
			#define FIRM_API extern __declspec(dllimport)
		#else
			#define FIRM_API
		#endif
	#endif
#else
	#define  FIRM_API
#endif

/* mark declarations as C function */
#ifdef __cplusplus
extern "C" {
#endif

#endif
