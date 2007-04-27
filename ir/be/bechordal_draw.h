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
 * @brief       Paint chordal graphs.
 * @author      Sebastian Hack
 * @date        12.05.2005
 * @version     $Id$
 */
#ifndef FIRM_BE_BECHORDAL_DRAW_H
#define FIRM_BE_BECHORDAL_DRAW_H

#include "bechordal.h"

typedef struct _plotter_t           plotter_t;
typedef struct _plotter_if_t        plotter_if_t;
typedef struct _rect_t              rect_t;
typedef struct _draw_chordal_opts_t draw_chordal_opts_t;
typedef struct _color_t             color_t;

struct _color_t {
	double r, g, b;
};

struct _rect_t {
	int x, y, w, h;
};

struct _plotter_if_t {
	void (*begin)(plotter_t *self, const rect_t *visible_area);

	void (*set_color)(plotter_t *self, const color_t * color);
	const color_t *(*get_color)(const plotter_t *self);
	void (*set_width)(plotter_t *self, int width);
	int (*get_width)(const plotter_t *self);
	void (*line)(plotter_t *self, int x1, int y1, int x2, int y2);
	void (*box)(plotter_t *self, const rect_t *rect);
	void (*text)(plotter_t *self, int x, int y, const char *str);

	void (*finish)(plotter_t *self);
	void (*free)(plotter_t *self);
};

extern void plotter_free(plotter_t *self);

struct _plotter_t {
	const plotter_if_t *vtab;
};

struct _draw_chordal_opts_t {
	int h_gap;
	int h_inter_gap;
	int v_gap;
	int v_inter_gap;
	int x_margin;
	int y_margin;
};

extern const draw_chordal_opts_t draw_chordal_def_opts;

extern plotter_t *new_plotter_ps(const char *filename);

extern void draw_interval_tree(const draw_chordal_opts_t *opts, const be_chordal_env_t *chordal_env, plotter_t *plotter);

#endif /* FIRM_BE_BECHORDAL_DRAW_H */
