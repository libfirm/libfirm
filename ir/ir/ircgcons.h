/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgcons.h
 * Purpose:     Construction and removal of interprocedural representation
 *              (explicit interprocedural dependencies).
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef _CONSTRUCT_H_
#define _CONSTRUCT_H_


#include "entity.h"


/* Aufbau der interprozeduralen Darstellung.  In den Call-Operationen
 * muessen alle potentiellen callees gespeichert sein. */
void cg_construct(int arr_len, entity *free_methods_arr[]);


/* Abbau der interprozeduralen (Sichten-) Darstellung, in eine
 * gewoehnliche intraprozedurale Darstellung */
void cg_destruct(void);


#endif /* _CONSTRUCT_H_ */
