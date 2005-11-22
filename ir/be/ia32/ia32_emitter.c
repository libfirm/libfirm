#include "tv.h"
#include "iredges.h"
#include "ia32_emitter.h"
#include "ia32_nodes_attr.h"
#include "ia32_new_nodes.h"

#define TARVAL_SNPRINTF_BUF_LEN 1024

const char *get_dest_reg_name(ir_node *n, int num) {
  return get_ia32_out_reg_name(n, --num);
}

const char *get_source_reg_name(ir_node *n, int num) {
  return get_ia32_in_reg_name(n, --num);
}

char *node_const_to_str(ir_node *n) {
  char *buf = malloc(TARVAL_SNPRINTF_BUF_LEN);
  tarval_snprintf(buf, TARVAL_SNPRINTF_BUF_LEN, get_ia32_Immop_tarval(n));
  return buf;
}

char *node_offset_to_str(ir_node *n) {
  char *buf = malloc(TARVAL_SNPRINTF_BUF_LEN);
  tarval_snprintf(buf, TARVAL_SNPRINTF_BUF_LEN, get_ia32_offs(n));
  return buf;
}

void equalize_dest_src(FILE *F, ir_node *n) {
  if (get_ia32_out_regnr(n, 0) != get_ia32_in_regnr(n, 0))
    fprintf(F, "\tmovl %%%s, %%%s\t\t\t/* src -> dest for 2 address code */\n", get_source_reg_name(n, 1), get_dest_reg_name(n, 1));
}

/*
 * coding of conditions
 */
struct cmp2conditon_t {
  const char *name;
  pn_Cmp      num;
};

/*
 * positive conditions for signed compares
 */
static const struct cmp2conditon_t cmp2condition_s[] = {
  { NULL,              pn_Cmp_False },  /* always false */
  { "e",               pn_Cmp_Eq },     /* == */
  { "l",               pn_Cmp_Lt },     /* < */
  { "le",              pn_Cmp_Le },     /* <= */
  { "g",               pn_Cmp_Gt },     /* > */
  { "ge",              pn_Cmp_Ge },     /* >= */
  { "ne",              pn_Cmp_Lg },     /* != */
  { "ordered",         pn_Cmp_Leg },    /* Floating point: ordered */
  { "unordered",       pn_Cmp_Uo },     /* FLoting point: unordered */
  { "unordered or ==", pn_Cmp_Ue },     /* Floating point: unordered or == */
  { "unordered or <",  pn_Cmp_Ul },     /* Floating point: unordered or < */
  { "unordered or <=", pn_Cmp_Ule },    /* Floating point: unordered or <= */
  { "unordered or >",  pn_Cmp_Ug },     /* Floating point: unordered or > */
  { "unordered or >=", pn_Cmp_Uge },    /* Floating point: unordered or >= */
  { "unordered or !=", pn_Cmp_Ne },     /* Floating point: unordered or != */
  { NULL,              pn_Cmp_True },   /* always true */
};

/*
 * positive conditions for unsigned compares
 */
static const struct cmp2conditon_t cmp2condition_u[] = {
  { NULL,              pn_Cmp_False },  /* always false */
  { "e",               pn_Cmp_Eq },     /* == */
  { "b",               pn_Cmp_Lt },     /* < */
  { "be",              pn_Cmp_Le },     /* <= */
  { "a",               pn_Cmp_Gt },     /* > */
  { "ae",              pn_Cmp_Ge },     /* >= */
  { "ne",              pn_Cmp_Lg },     /* != */
  { "ordered",         pn_Cmp_Leg },    /* Floating point: ordered */
  { "unordered",       pn_Cmp_Uo },     /* FLoting point: unordered */
  { "unordered or ==", pn_Cmp_Ue },     /* Floating point: unordered or == */
  { "unordered or <",  pn_Cmp_Ul },     /* Floating point: unordered or < */
  { "unordered or <=", pn_Cmp_Ule },    /* Floating point: unordered or <= */
  { "unordered or >",  pn_Cmp_Ug },     /* Floating point: unordered or > */
  { "unordered or >=", pn_Cmp_Uge },    /* Floating point: unordered or >= */
  { "unordered or !=", pn_Cmp_Ne },     /* Floating point: unordered or != */
  { NULL,              pn_Cmp_True },   /* always true */
};

/*
 * returns the condition code
 */
const char *get_cmp_suffix(int cmp_code, int unsigned_cmp)
{
  assert(cmp2condition_s[cmp_code].num == cmp_code);
  assert(cmp2condition_u[cmp_code].num == cmp_code);

  return unsigned_cmp ? cmp2condition_u[cmp_code & 7].name : cmp2condition_s[cmp_code & 7].name;
}

void emit_ia32_Proj_Cond(FILE *F, ir_node *n, ir_node *cond) {
  ir_node *succ_block = get_edge_src_irn(get_irn_out_edge_first(n));
  ir_node *sel        = get_Cond_selector(cond);
  ir_mode *sel_mode   = get_irn_mode(sel);

  assert(succ_block && "Target block of Proj_Cond missing!");

  if (sel_mode == mode_b) { // Boolean condition
    int label = get_irn_node_nr(succ_block);
    int nr    = get_Proj_proj(n);
    fprintf(F, "j%s%s Label%d\t\t\t/* if (%sCond) goto Label */\n",
          nr == pn_Cond_true ? "" : "n",
          get_cmp_suffix(get_Proj_proj(sel), mode_is_signed(sel_mode)),
          label,
          nr == pn_Cond_true ? "" : "!");
  }
}
