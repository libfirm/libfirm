/* Magic cookies for dynamic data structures
   Copyright (C) 1995, 1996 Christian von Roques & Markus Armbruster
   All rights reserved.  */

/* $Id$ */

#ifndef _COOKIES_H
#define _COOKIES_H

/* Arrays */
#define ARR_D_MAGIC	0xD138A4
#define ARR_A_MAGIC	0xA138A4
#define ARR_F_MAGIC	0xF138A4

/* Pointer Double Ended Queue */
#define PDEQ_COOKIE1	0xC00B8A
#define PDEQ_COOKIE2	0x8A771E

/* Small Pointer DICTionary */
#define SPDICT_COOKIE1	0xB0031E7
#define SPDICT_COOKIE2	0xBA17B003

/* Line N_o_mbers ToDo: Warum o? Answer: No. 5, not Nu. 5*/
#define LINO_COOKIE	0x71b83bd5

#endif
