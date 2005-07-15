/**
 * Internal headers for liveness analysis.
 * @author Sebastian Hack
 * @date 6.12.2004
 */

#ifndef _BELIVE_T_H
#define _BELIVE_T_H

#include "config.h"

#include "irgraph_t.h"
#include "iredges_t.h"

#include "belive.h"
#include "pset.h"
#include "set.h"
#include "list.h"
#include "hashptr.h"

typedef enum _live_state_t {
    live_state_in = 1,
    live_state_end = 2,
    live_state_out = 4,
    live_state_block = 8,
} live_state_t;

typedef struct _irn_live_t {
    const ir_node *block;
    const ir_node *irn;
    unsigned state;
    struct _irn_live_t *next;
} irn_live_t;

typedef struct _irg_live_info_t {
    set *live;
} irg_live_info_t;

extern size_t live_irg_data_offset;

#define get_irg_live_info(irg) (get_irg_data(irg, irg_live_info_t, live_irg_data_offset))

#define live_is_in(live) (((live)->state & live_state_in) != 0)
#define live_is_end(live) (((live)->state & live_state_end) != 0)
#define live_is_out(live) (((live)->state & live_state_out) != 0)

static INLINE irn_live_t *_get_or_set_live(const ir_node *block,
    const ir_node *irn, int state)
{
  irg_live_info_t *live_info = get_irg_live_info(get_irn_irg(block));
  irn_live_t *live, templ;
  unsigned hash = HASH_PTR(block) + 37 * HASH_PTR(irn);

  templ.block = block;
  templ.irn = irn;
  templ.state = -1;
  templ.next = NULL;

  live = set_insert(live_info->live, &templ, sizeof(templ), hash);
  if(live->state == -1) {

    if(!is_Block(irn)) {
      irn_live_t *bl_live = _get_or_set_live(block, block, live_state_block);
      live->next = bl_live->next;
      bl_live->next = live;
    }

    live->state = state;
  }

  live->state |= state;

  return live;
}

static INLINE int _is_live_in(const ir_node *block, const ir_node *irn)
{
    return (_get_or_set_live(block, irn, 0)->state & live_state_in) != 0;
}

static INLINE int _is_live_out(const ir_node *block, const ir_node *irn)
{
    return (_get_or_set_live(block, irn, 0)->state & live_state_out) != 0;
}

static INLINE int _is_live_end(const ir_node *block, const ir_node *irn)
{
    return (_get_or_set_live(block, irn, 0)->state & live_state_end) != 0;
}

#define live_foreach(block, live_info) \
	for(live_info = _get_or_set_live(block, block, 0)->next; live_info; live_info = live_info->next)

static INLINE void _put_live(const ir_node *block, int state, pset *s)
{
    irn_live_t *live;

    live_foreach(block, live) {
        if(live->state & state)
            pset_insert_ptr(s, live->irn);
    }
}

static INLINE pset *_put_live_in(const ir_node *block, pset *s)
{
    _put_live(block, live_state_in, s);
    return s;
}

static INLINE pset *_put_live_out(const ir_node *block, pset *s)
{
    _put_live(block, live_state_out, s);
    return s;
}

static INLINE pset *_put_live_end(const ir_node *block, pset *s)
{
    _put_live(block, live_state_end, s);
    return s;
}

static INLINE int _is_phi_arg(const ir_node *irn)
{
  const ir_edge_t *edge;

  assert(edges_activated(get_irn_irg(irn)) && "Please compute the out edges");
  foreach_out_edge(irn, edge)
    if(is_Phi(edge->src))
      return 1;

  return 0;
}


#define is_live_in(bl,irn) 			_is_live_in(bl, irn)
#define is_live_out(bl,irn) 		_is_live_out(bl, irn)
#define is_live_end(bl,irn) 		_is_live_end(bl, irn)
#define put_live_in(bl,s)			  _put_live_in(bl, s)
#define put_live_out(bl,s)			_put_live_out(bl, s)
#define put_live_end(bl,s)			_put_live_end(bl, s)
#define is_phi_arg(irn)         _is_phi_arg(irn)

/**
 * Initialize the liveness module.
 * To be called from be_init().
 */
void be_liveness_init(void);

#endif
