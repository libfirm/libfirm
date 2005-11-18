#include "iredges.h"
#include "emitter.h"

char *get_dest_reg_name(ir_node *n, int num) {
return NULL;
}

char *get_source_reg_name(ir_node *n, int num) {
return NULL;
}

char *node_const_to_str(ir_node *n) {
return NULL;
}

char *node_offset_to_str(ir_node *n) {
return NULL;
}

void equalize_dest_src(ir_node *n) {
#if 0
  if (get_dest_reg(n, 1) != get_source_reg(n, 1))
    fprintf("\tmovl %%%s, %%%s\t\t\t/* src -> dest for 2 address code */\n", get_source_reg_name(n, 1), get_dest_reg_name(n, 1));
#endif
}

/*
 * coding of conditions
 */
struct cmp2conditon_t {
  const char 	*name;
  pn_Cmp 	num;
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
#if 0
  ir_node *succ_block = get_irn_out_edges_first(n);
  ir_node *sel        = get_Cond_selector(cond);
  ir_mode *sel_mode   = get_irn_mode(sel);

  assert(succ_block && "Target block of Proj_Cond missing!");

  if (sel_mode == mode_b) { // Boolean condition
    int label = get_irn_node_nr(succ_block);
    int nr    = get_Proj_proj(n);
    fprintf(F, "j%s%s Label%d\t\t\t/* if (%sCond) goto Label */\n",
          nr == pn_Cond_true ? "" : "n",
          get_cmp_suffix(get_Proj_proj(sel), mode_is_signed(cmp mode)),
          label,
          nr == pn_Cond_true ? "" : "!");
  }
#endif
}
