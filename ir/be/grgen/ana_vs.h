/*
 * Project:     libFIRM/extension module/graph rewriting system
 * File name:   ext/grs/ana_vs.h
 * Purpose:		provides an interface for getting a v-structure based analyzer
 *
 * Author:      Veit Batz
 * Created:		22. June 2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file	ext/grs/ana_vs.h
 * @brief	provides an interface for getting a v-structure based analyzer
 */

#ifndef _EXT_GRS_ANA_VS_H_
#define _EXT_GRS_ANA_VS_H_




#include "analyze.h"



/** yields an analyzer perorming a strong v structure statistic */
ext_grs_analyzer_t *ext_grs_get_vs_analyzer(void);




#endif /*_EXT_GRS_ANA_VS_H_*/
