/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief       Paint chordal graphs.
 * @author      Sebastian Hack
 * @date        12.05.2005
 */
#ifndef FIRM_BE_BECHORDAL_DRAW_H
#define FIRM_BE_BECHORDAL_DRAW_H

#include "bechordal.h"

typedef struct plotter_t           plotter_t;
typedef struct plotter_if_t        plotter_if_t;
typedef struct rect_t              rect_t;
typedef struct draw_chordal_opts_t draw_chordal_opts_t;
typedef struct color_t             color_t;

struct color_t {
	double r, g, b;
};

struct rect_t {
	int x, y, w, h;
};

struct plotter_if_t {
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

struct plotter_t {
	const plotter_if_t *vtab;
};

struct draw_chordal_opts_t {
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

#endif
