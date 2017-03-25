/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Should be included in all public firm headers at the beginning
 * @author      Matthias Braun
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
			#define FIRM_API extern
		#endif
	#endif
#else
	#define  FIRM_API extern
#endif

#endif

/* mark declarations as C function (note that we always need this,
 * so don't put it in the include guard) */
#ifdef __cplusplus
extern "C" {
#endif
