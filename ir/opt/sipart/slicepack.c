#include <assert.h>
#include <firm.h>
#include <stdint.h>

#include "irnode_t.h"
#include "sitools.h"
#include "irverify.h"
#include <debug.h>

/* XXX: TODO: BUG: The Slice/Pack lowering by pattern matching was not
 * used for the evaluation since graphs were observed containing Slice
 * nodes that sliced up to bit 33 out of a 32 bit value and touching
 * the code was a no-go due to deadlines.
 *
 * Should be easy to debug though:
 *  1. Modify verify_node_Slice to detect this case
 *  2. Trigger ir_verify assertion using libfirm/test/sipart/cases/
 *  3. Binary search for the culprit lower_slice_walker_X
 */

DEBUG_ONLY(static firm_dbg_module_t *dbg;)
#define panic(x) do { puts(x); exit(-1);  } while(0)

/* Creates a bit mask that have the msb and all less significant bits set.
   (stolen from dca.c) */
static ir_tarval *create_msb_mask(ir_tarval *tv)
{
	unsigned shift_amount = 1;
	for (int msb = get_tarval_highest_bit(tv); msb != 0; msb /= 2) {
		tv            = tarval_or(tv, tarval_shr_unsigned(tv, shift_amount));
		shift_amount *= 2;
	}

	return tv;
}

/* Creates a bit mask that have the lsb and all more significant bits set. */
static ir_tarval *create_lsb_mask(ir_tarval *tv)
{
	return tarval_or(tv, tarval_neg(tv));
}


static ir_node *skip_upconv(ir_node *node)
{
  if (!is_Conv(node)) return node;

  ir_node *conv = node;
  ir_mode *conv_mode = get_irn_mode(node);

  ir_node *pred = get_Conv_op(conv);
  ir_mode *pred_mode = get_irn_mode(pred);

  if (get_mode_size_bits(conv_mode) >= get_mode_size_bits(pred_mode))
    return pred;
  else
    return node;
}

#define turn_into_slice_d(node, value, from, to)  \
  DB2("%s: ", __func__); \
  turn_into_slice(node, value, from, to);

static ir_mode *get_vhdl_mode(int width, int sign)
{
  char buf[64];
  snprintf(buf, 64, "V%d%c", width, sign ? 's' : 'u');
  return new_int_mode(buf, irma_twos_complement, width, sign, 32);
}

static void turn_into_slice(ir_node *node, ir_node *value, int from, int to)
{
  ir_mode *slice_mode = get_vhdl_mode(to-from+1, 0);
  ir_mode *mode = get_irn_mode(node);
  add_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
  set_cur_block(get_nodes_block(node));

  ir_node* slice = new_Slice(value, slice_mode, from, to);
  DB2("%+F --> %+F(%+F[%d..%d])\n", node, slice, get_Slice_pred(slice), get_Slice_from(slice), get_Slice_to(slice));

  set_nodes_block(slice, get_nodes_block(node));
  if (slice_mode != mode)
    slice = new_Conv(slice, mode);
  exchange(node, slice);

  clear_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
}


/* And(Shrs(value, Const), Const(mask)) -> Slice(value) */
irg_walk_func lower_slice_walker;
void lower_slice_walker(ir_node *node, void *data) {
    (void)data;
  /* ir_node *conv = node; */
  /* if (!is_Conv(conv)) return; */
  /* ir_node *and = get_Conv_op(conv); */
  ir_node *and = node;
  if (!is_And(and)) return;
  ir_node *mask = get_And_right(and);
  if (!is_Const(mask)) return;
  ir_node *value = get_And_left(and);

  /* Check whether the mask is a proper interval. */
  ir_tarval *mask_tv = get_Const_tarval(mask);

  if (mask_tv != create_msb_mask(mask_tv))
    return;
  if (0 != get_tarval_lowest_bit(mask_tv))
    return;

  int from = 0;
  if (is_Shrs(value)) {
    ir_node *count = get_Shrs_right(value);
    if (is_Const(count)) {
      from = get_tarval_long(get_Const_tarval(count));
      value = get_Shrs_left(value);
    } else return;
  }

  int to = from + get_tarval_highest_bit(mask_tv);
  if (from > to) return;
  turn_into_slice_d(and, value, from, to);
}

/* Shr(And(value, Const(mask)), count) -> Slice(value) */
irg_walk_func lower_slice_walker_3;
void lower_slice_walker_3(ir_node *shr, void *data) {
    (void)data;
  if (!is_Shr(shr))
    return;
  ir_node *count = get_Shr_right(shr);
  if (!is_Const(count))
    return;
  long count_long = get_tarval_long(get_Const_tarval(count));
  ir_node *and = get_Shr_left(shr);
  if (!is_And(and))
    return;
  ir_node *mask = get_And_right(and);
  if (!is_Const(mask))
    return;

  ir_tarval *mask_tv = get_Const_tarval(mask);
  ir_node *value = get_And_left(and);

  /* Check whether the mask is a proper interval. */

  ir_tarval *msb_mask = create_msb_mask(mask_tv);
  ir_tarval *lsb_mask = create_lsb_mask(mask_tv);
  if (tarval_and(msb_mask, lsb_mask) != mask_tv)
    return;

  int from = get_tarval_lowest_bit(mask_tv);
  int to = get_tarval_highest_bit(mask_tv);

  if (from > to) return;
  if (count_long != from) return;

  turn_into_slice_d(shr, value, from, to);
}

/* FIXME: bogus when sign makes it into slice. */
/* Shrs(And(value, Const(mask)), count) -> Slice(value) */
irg_walk_func lower_slice_walker_4;
void lower_slice_walker_4(ir_node *shrs, void *data) {
    (void)data;
  if (!is_Shrs(shrs))
    return;
  ir_node *count = get_Shrs_right(shrs);
  if (!is_Const(count))
    return;
  long count_long = get_tarval_long(get_Const_tarval(count));
  ir_node *and = get_Shrs_left(shrs);
  if (!is_And(and))
    return;
  ir_node *mask = get_And_right(and);
  if (!is_Const(mask))
    return;

  ir_tarval *mask_tv = get_Const_tarval(mask);
  ir_node *value = get_And_left(and);

  /* Check whether the mask is a proper interval. */

  ir_tarval *msb_mask = create_msb_mask(mask_tv);
  ir_tarval *lsb_mask = create_lsb_mask(mask_tv);
  if (tarval_and(msb_mask, lsb_mask) != mask_tv)
    return;

  int from = get_tarval_lowest_bit(mask_tv);
  int to = get_tarval_highest_bit(mask_tv);

  if (from > to) return;
  if (count_long != from) return;

  turn_into_slice_d(shrs, value, from, to);
}

/* stolen from Martin */
static void node_dump_Slice(FILE *out, const ir_node *self, dump_reason_t reason) {
  ir_node *pred;
  size_t slice_from, slice_to;
  ir_mode* mode;

  switch(reason) {
  case dump_node_opcode_txt:
    fprintf(out, "%s[%li..%li]", get_irn_opname(self), get_Slice_from(self), get_Slice_to(self) );
    break;

  case dump_node_mode_txt:
    mode = get_irn_mode(self);

    if (mode != NULL && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T )
      fprintf(out, "%s", get_mode_name(mode));
    break;

  case dump_node_nodeattr_txt:
    //keep empty - nodenr inserted automagically
    break;

  case dump_node_info_txt:
    pred    = get_Slice_pred(self);
    slice_from = get_Slice_from(self);
    slice_to   = get_Slice_to(self);
    fprintf(out, "\t%s[%li..%li]\n", get_irn_opname(self), slice_from, slice_to);
    ir_fprintf(out, "Pred: %+F\n", pred);
    break;

  default:
    DBG((dbg, LEVEL_DEFAULT, "unknown dump_node_kind\n"));
  }
}

irg_walk_func lower_pack_walker;
void lower_pack_walker(ir_node *node, void *data) {
  (void)data;
  if (!(is_Or(node) || is_Add(node))) return;
  ir_mode *mode = get_irn_mode(node);
  ir_node *left = get_binop_left(node);
  ir_node *right = get_binop_right(node);

  ir_node *low, *high;

  if (is_Conv(left)) {
    low = get_Conv_op(left);
    high = right;
  } else if (is_Conv(right)) {
    low = get_Conv_op(right);
    high = left;
  } else
    return;

  ir_mode *low_mode = get_irn_mode(low);

  long shiftbits = 0;
  if (is_Shl(high)) {
	ir_node *shift_count = get_Shl_right(high);

	if (!is_Const(shift_count)) return;
	shiftbits = get_tarval_long(get_Const_tarval(shift_count));
	if (!get_mode_size_bits(low_mode) == shiftbits) return;
  } else if (is_Mul(high)) {
	ir_node *constnode = get_Mul_right(high);
	if (!is_Const(constnode)) return;
	ir_tarval *tv = get_Const_tarval(constnode);
	shiftbits = get_tarval_highest_bit(tv);
	if (get_tarval_lowest_bit(tv) != shiftbits) return;
  } else {
	return;
  }

  high = get_binop_left(high);
  if (is_Conv(high))
    high = get_Conv_op(high);

  ir_mode *high_mode = get_irn_mode(high);
  if (get_mode_size_bits(mode) !=
      (get_mode_size_bits(high_mode) + get_mode_size_bits(low_mode))) {

    add_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
    ir_mode *slice_mode = get_vhdl_mode(get_mode_size_bits(high_mode)-shiftbits, 0 /* FIXME */);
    high = new_Slice(high, slice_mode, shiftbits, get_mode_size_bits(high_mode)-1);
    clear_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
  }

  add_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
  set_cur_block(get_nodes_block(node));

  {
    ir_node *in[2];
    in[0] = low;
    in[1] = high;

    ir_node* pack = new_Pack(2, in, mode);

    DB2("made %+F: packing %+F and %+F\n", pack, low, high);

    set_nodes_block(pack, get_nodes_block(node));
    exchange(node, pack);

    clear_irg_constraints(get_irn_irg(node), IR_GRAPH_CONSTRAINT_CONSTRUCTION);
  }
}

/**
 * Lower bit operations into slice and pack nodes.
 * @param irg The graph to perform the lowering on.
 */
void lower_slice_pack(ir_graph *irg)
{
  FIRM_DBG_REGISTER(dbg, "firm.si.slicepack");
  bool modified=0;
	set_op_dump(op_Slice, node_dump_Slice);
  /* set_op_hash(op_Address, hash_entconst); */

  irg_walk_graph(irg, lower_slice_walker, NULL, &modified);
  irg_walk_graph(irg, lower_slice_walker_3, NULL, &modified);
  irg_walk_graph(irg, lower_slice_walker_4, NULL, &modified);
  irg_walk_graph(irg, lower_pack_walker, NULL, &modified);
}

