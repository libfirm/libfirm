/**
 * @file irarch.c
 * @date 28.9.2004
 * @author Sebastian Hack
 * @brief Machine dependent firm optimizations.
 *
 * $Id$
 */
#include <stdlib.h>
#include <assert.h>

#include "irnode_t.h"
#include "irgraph_t.h"
#include "irmode_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgmod.h"
#include "irvrfy.h"
#include "tv.h"
#include "dbginfo_t.h"
#include "iropt_dbg.h"
#include "irflag_t.h"
#include "firmstat.h"
#include "ircons.h"
#include "irarch.h"
#include "firmstat.h"

#undef DEB

#define MAX_BITSTR 64

/* when we need verifying */
#ifdef NDEBUG
# define IRN_VRFY_IRG(res, irg)
#else
# define IRN_VRFY_IRG(res, irg)  irn_vrfy_irg(res, irg)
#endif

/** The params got from the factory in arch_dep_init(...). */
static const arch_dep_params_t *params = NULL;

/** The bit mask, which optimizations to apply. */
static arch_dep_opts_t opts;

/* we need this new pseudo op */
static ir_op *op_Mulh = NULL;

/**
 * construct a Mulh: Mulh(a,b) = (a * b) >> w, w is the with in bits of a, b
 */
static ir_node *
new_rd_Mulh (dbg_info *db, ir_graph *irg, ir_node *block,
       ir_node *op1, ir_node *op2, ir_mode *mode)
{
  ir_node *in[2];
  ir_node *res;

  if (! op_Mulh) {
    op_Mulh = new_ir_op(get_next_ir_opcode(), "Mulh",  op_pin_state_floats, irop_flag_commutative, oparity_binary, 0, 0);
  }

  in[0] = op1;
  in[1] = op2;
  res = new_ir_node(db, irg, block, op_Mulh, mode, 2, in);
  res = optimize_node(res);
  IRN_VRFY_IRG(res, irg);
  return res;
}

ir_op *get_op_Mulh(void)  { return op_Mulh; }

void arch_dep_init(arch_dep_params_factory_t factory)
{
  opts = arch_dep_none;

  if (factory != NULL)
    params = factory();

  if (params && (opts & (arch_dep_div_by_const|arch_dep_mod_by_const))) {
    if (! op_Mulh) {
      /* create the Mulh operation */
      op_Mulh = new_ir_op(get_next_ir_opcode(), "Mulh",  op_pin_state_floats, irop_flag_commutative, oparity_binary, 0, 0);
    }
  }
}

void arch_dep_set_opts(arch_dep_opts_t the_opts) {
  opts = the_opts;
}

ir_node *arch_dep_replace_mul_with_shifts(ir_node *irn)
{
  ir_node *res = irn;
  ir_mode *mode = get_irn_mode(irn);

  /* If the architecture dependent optimizations were not initialized
     or this optimization was not enabled. */
  if(params == NULL || (opts & arch_dep_mul_to_shift) == 0)
    return irn;

  if(get_irn_opcode(irn) == iro_Mul && mode_is_int(mode)) {
    ir_node *block   = get_nodes_block(irn);
    ir_node *left    = get_binop_left(irn);
    ir_node *right   = get_binop_right(irn);
    tarval *tv       = NULL;
    ir_node *operand = NULL;

    /* Look, if one operand is a constant. */
    if(get_irn_opcode(left) == iro_Const) {
      tv = get_Const_tarval(left);
      operand = right;
    } else if(get_irn_opcode(right) == iro_Const) {
      tv = get_Const_tarval(right);
      operand = left;
    }

    if(tv != NULL) {
      int maximum_shifts = params->maximum_shifts;
      int also_use_subs = params->also_use_subs;
      int highest_shift_amount = params->highest_shift_amount;

      char *bitstr = get_tarval_bitpattern(tv);
      char *p;
      int i, last = 0;
      int counter = 0;
      int curr_bit;
      int compr_len = 0;
      char compr[MAX_BITSTR];

      int singleton;
      int end_of_group;
      int shift_with_sub[MAX_BITSTR] = { 0 };
      int shift_without_sub[MAX_BITSTR] = { 0 };
      int shift_with_sub_pos = 0;
      int shift_without_sub_pos = 0;

#if DEB
      {
        long val = get_tarval_long(tv);
        fprintf(stderr, "Found mul with %ld(%lx) = ", val, val);
        for(p = bitstr; *p != '\0'; p++)
          printf("%c", *p);
        printf("\n");
      }
#endif

      for(p = bitstr; *p != '\0'; p++) {
        int bit = *p != '0';

        if (bit != last) {
          /* The last was 1 we are now at 0 OR
           * The last was 0 and we are now at 1 */
          compr[compr_len++] = counter;
          counter = 1;
        }
        else
          counter++;

        last = bit;
      }
      compr[compr_len++] = counter;


#ifdef DEB
      {
        const char *prefix = "";
        for(i = 0; i < compr_len; i++, prefix = ",")
          fprintf(stderr, "%s%d", prefix, compr[i]);
        fprintf("\n");
      }
#endif

      // Go over all recorded one groups.
      curr_bit = compr[0];

      for(i = 1; i < compr_len; i = end_of_group + 2) {
        int j, zeros_in_group, ones_in_group;

        ones_in_group = compr[i];
        zeros_in_group = 0;

        // Scan for singular 0s in a sequence
        for(j = i + 1; j < compr_len && compr[j] == 1; j += 2) {
          zeros_in_group += 1;
          ones_in_group += (j + 1 < compr_len ? compr[j + 1] : 0);
        }
        end_of_group = j - 1;

        if(zeros_in_group >= ones_in_group - 1)
          end_of_group = i;

#ifdef DEB
        fprintf(stderr, "  i:%d, eg:%d\n", i, end_of_group);
#endif

        singleton = compr[i] == 1 && i == end_of_group;
        for(j = i; j <= end_of_group; j += 2) {
          int curr_ones = compr[j];
          int biased_curr_bit = curr_bit + 1;
          int k;

#ifdef DEB
          fprintf(stderr, "    j:%d, ones:%d\n", j, curr_ones);
#endif

          // If this ones group is a singleton group (it has no
          // singleton zeros inside
          if(singleton)
            shift_with_sub[shift_with_sub_pos++] = biased_curr_bit;
          else if(j == i)
            shift_with_sub[shift_with_sub_pos++] = -biased_curr_bit;

          for(k = 0; k < curr_ones; k++)
            shift_without_sub[shift_without_sub_pos++] = biased_curr_bit + k;

          curr_bit += curr_ones;
          biased_curr_bit = curr_bit + 1;

          if(!singleton && j == end_of_group)
            shift_with_sub[shift_with_sub_pos++] = biased_curr_bit;
          else if(j != end_of_group)
            shift_with_sub[shift_with_sub_pos++] = -biased_curr_bit;

          curr_bit += compr[j + 1];
        }

      }

      {
        int *shifts = shift_with_sub;
        int n = shift_with_sub_pos;
        int highest_shift_wide = 0;
        int highest_shift_seq = 0;
        int last_shift = 0;

        /* If we may not use subs, or we can achive the same with adds,
           prefer adds. */
        if(!also_use_subs || shift_with_sub_pos >= shift_without_sub_pos) {
          shifts = shift_without_sub;
          n = shift_without_sub_pos;
        }

        /* If the number of needed shifts exceeds the given maximum,
           use the Mul and exit. */
        if(n > maximum_shifts) {
#ifdef DEB
          fprintf(stderr, "Only allowed %d shifts, but %d are needed\n",
              maximum_shifts, n);
#endif
          return irn;
        }

        /* Compute the highest shift needed for both, the
           sequential and wide representations. */
        for(i = 0; i < n; i++) {
          int curr = abs(shifts[i]);
          int curr_seq = curr - last;

          highest_shift_wide = curr > highest_shift_wide ? curr
            : highest_shift_wide;
          highest_shift_seq = curr_seq > highest_shift_seq ? curr_seq
            : highest_shift_seq;

          last_shift = curr;
        }

        /* If the highest shift amount is greater than the given limit,
           give back the Mul */
        if(highest_shift_seq > highest_shift_amount) {
#ifdef DEB
          fprintf(stderr, "Shift argument %d exceeds maximum %d\n",
              highest_shift_seq, highest_shift_amount);
#endif
          return irn;
        }

        /* If we have subs, we cannot do sequential. */
        if(1 /* also_use_subs */) {
          if(n > 0) {
            ir_node *curr = NULL;

            i = n - 1;

            do {
              int curr_shift = shifts[i];
              int sub = curr_shift < 0;
              int amount = abs(curr_shift) - 1;
              ir_node *aux = operand;


              assert(amount >= 0 && "What is a negative shift??");

              if(amount != 0) {
                tarval *shift_amount = new_tarval_from_long(amount, mode_Iu);
                ir_node *cnst = new_r_Const(current_ir_graph, block, mode_Iu, shift_amount);
                aux = new_r_Shl(current_ir_graph, block, operand, cnst, mode);
              }

              if(curr) {
                if(sub)
                  curr = new_r_Sub(current_ir_graph, block, curr, aux, mode);
                else
                  curr = new_r_Add(current_ir_graph, block, curr, aux, mode);
              } else
                curr = aux;

            } while(--i >= 0);

            res = curr;
          }
        }

#ifdef DEB
        {
          const char *prefix = "";
          for(i = 0; i < n; i++) {
            fprintf(stderr, "%s%d", prefix, shifts[i]);
            prefix = ", ";
          }
          fprintf(stderr, "\n");
        }
#endif

      }

      if(bitstr)
        free(bitstr);
    }

  }

  if (res != irn)
    stat_arch_dep_replace_mul_with_shifts(irn);

  return res;
}

/**
 * calculated the ld2 of a tarval if tarval is 2^n, else returns -1.
 */
static int tv_ld2(tarval *tv, int bits)
{
  int i, k, num;

  for (num = i = 0; i < bits; ++i) {
    unsigned char v = get_tarval_sub_bits(tv, i);

    if (v) {
      int j;

      for (j = 0; j < 8; ++j)
        if ((1 << j) & v) {
          ++num;
          k = 8 * i + j;
        }
    }
  }
  if (num == 1)
    return k;
  return -1;
}


/* for shorter lines */
#define ABS(a)    tarval_abs(a)
#define NEG(a)    tarval_neg(a)
#define NOT(a)    tarval_not(a)
#define SHL(a, b) tarval_shl(a, b)
#define SHR(a, b) tarval_shr(a, b)
#define ADD(a, b) tarval_add(a, b)
#define SUB(a, b) tarval_sub(a, b)
#define MUL(a, b) tarval_mul(a, b)
#define DIV(a, b) tarval_div(a, b)
#define MOD(a, b) tarval_mod(a, b)
#define CMP(a, b) tarval_cmp(a, b)
#define CNV(a, m) tarval_convert_to(a, m)
#define ONE(m)    get_mode_one(m)
#define ZERO(m)   get_mode_null(m)

struct ms {
  tarval *M;        /**< magic number */
  int s;            /**< shift amount */
  int need_add;     /**< an additional add is needed */
  int need_sub;     /**< an additional sub is needed */
};

/**
 * Signed division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Hacker's Delight: 10-6 Integer Division by Constants: Incorporation into a Compiler
 */
static struct ms magic(tarval *d)
{
  ir_mode *mode   = get_tarval_mode(d);
  ir_mode *u_mode = find_unsigned_mode(mode);
  int bits        = get_mode_size_bits(u_mode);
  int p;
  tarval *ad, *anc, *delta, *q1, *r1, *q2, *r2, *t;     /* unsigned */
  pnc_number d_cmp, M_cmp;

  struct ms mag;

  /* 2^(bits-1) */
  tarval *bits_minus_1 = new_tarval_from_long(bits - 1, u_mode);
  tarval *two_bits_1   = SHL(get_mode_one(u_mode), bits_minus_1);

  ad  = CNV(ABS(d), u_mode);
  t   = ADD(two_bits_1, SHR(CNV(d, u_mode), bits_minus_1));
  anc = SUB(SUB(t, ONE(u_mode)), MOD(t, ad));   /* Absolute value of nc */
  p   = bits - 1;                               /* Init: p */
  q1  = DIV(two_bits_1, anc);                   /* Init: q1 = 2^p/|nc| */
  r1  = SUB(two_bits_1, MUL(q1, anc));          /* Init: r1 = rem(2^p, |nc|) */
  q2  = DIV(two_bits_1, ad);                    /* Init: q2 = 2^p/|d| */
  r2  = SUB(two_bits_1, MUL(q2, ad));           /* Init: r2 = rem(2^p, |d|) */

  do {
    ++p;
    q1 = ADD(q1, q1);                           /* Update q1 = 2^p/|nc| */
    r1 = ADD(r1, r1);                           /* Update r1 = rem(2^p, |nc|) */

    if (CMP(r1, anc) & Ge) {
      q1 = ADD(q1, ONE(u_mode));
      r1 = SUB(r1, anc);
    }

    q2 = ADD(q2, q2);                           /* Update q2 = 2^p/|d| */
    r2 = ADD(r2, r2);                           /* Update r2 = rem(2^p, |d|) */

    if (CMP(r2, ad) & Ge) {
      q2 = ADD(q2, ONE(u_mode));
      r2 = SUB(r2, ad);
    }

    delta = SUB(ad, r2);
  } while (CMP(q1, delta) & Lt || (CMP(q1, delta) & Eq && CMP(r1, ZERO(u_mode)) & Eq));

  d_cmp = CMP(d, ZERO(mode));

  if (d_cmp & Ge)
    mag.M = ADD(CNV(q2, mode), ONE(mode));
  else
    mag.M = SUB(ZERO(mode), ADD(CNV(q2, mode), ONE(mode)));

  M_cmp = CMP(mag.M, ZERO(mode));

  mag.s = p - bits;

  /* need an add if d > 0 && M < 0 */
  mag.need_add = d_cmp & Gt && M_cmp & Lt;

  /* need a sub if d < 0 && M > 0 */
  mag.need_sub = d_cmp & Lt && M_cmp & Gt;

  return mag;
}

struct mu {
  tarval *M;        /**< magic add constant */
  int s;            /**< shift amount */
  int need_add;     /**< add indicator */
};

/**
 * Unsigned division by constant d: calculate the Magic multiplier M and the shift amount s
 *
 * see Hacker's Delight: 10-10 Integer Division by Constants: Incorporation into a Compiler (Unsigned)
 */
static struct mu magicu(tarval *d)
{
  ir_mode *mode   = get_tarval_mode(d);
  int bits        = get_mode_size_bits(mode);
  int p;
  tarval *nc, *delta, *q1, *r1, *q2, *r2;

  struct mu magu;

  tarval *bits_minus_1 = new_tarval_from_long(bits - 1, mode);
  tarval *two_bits_1   = SHL(get_mode_one(mode), bits_minus_1);
  tarval *seven_ff     = SUB(two_bits_1, ONE(mode));

  magu.need_add = 0;                            /* initialize the add indicator */
  nc = SUB(NEG(ONE(mode)), MOD(NEG(d), d));
  p  = bits - 1;                                /* Init: p */
  q1 = DIV(two_bits_1, nc);                     /* Init: q1 = 2^p/nc */
  r1 = SUB(two_bits_1, MUL(q1, nc));            /* Init: r1 = rem(2^p, nc) */
  q2 = DIV(seven_ff, d);                        /* Init: q2 = (2^p - 1)/d */
  r2 = SUB(seven_ff, MUL(q2, d));               /* Init: r2 = rem(2^p - 1, d) */

  do {
    ++p;
    if (CMP(r1, SUB(nc, r1)) & Ge) {
      q1 = ADD(ADD(q1, q1), ONE(mode));
      r1 = SUB(ADD(r1, r1), nc);
    }
    else {
      q1 = ADD(q1, q1);
      r1 = ADD(r1, r1);
    }

    if (CMP(ADD(r2, ONE(mode)), SUB(d, r2)) & Ge) {
      if (CMP(q2, seven_ff) & Ge)
        magu.need_add = 1;

      q2 = ADD(ADD(q2, q2), ONE(mode));
      r2 = SUB(ADD(ADD(r2, r2), ONE(mode)), d);
    }
    else {
      if (CMP(q2, two_bits_1) & Ge)
        magu.need_add = 1;

      q2 = ADD(q2, q2);
      r2 = ADD(ADD(r2, r2), ONE(mode));
    }
    delta = SUB(SUB(d, ONE(mode)), r2);
  } while (p < 2*bits &&
          (CMP(q1, delta) & Lt || (CMP(q1, delta) & Eq && CMP(r1, ZERO(mode)) & Eq)));

  magu.M = ADD(q2, ONE(mode));                       /* Magic number */
  magu.s = p - bits;                                 /* and shift amount */

  return magu;
}

/**
 * build the Mulh replacement code for n / tv
 *
 * Note thet 'div' might be a mod or DivMod operation as well
 */
static ir_node *replace_div_by_mulh(ir_node *div, tarval *tv)
{
  dbg_info *dbg  = get_irn_dbg_info(div);
  ir_node *n     = get_binop_left(div);
  ir_node *block = get_nodes_block(div);
  ir_mode *mode  = get_irn_mode(n);
  int bits       = get_mode_size_bits(mode);
  ir_node *q, *t, *c;

  if (mode_is_signed(mode)) {
    struct ms mag = magic(tv);

    /* generate the Mulh instruction */
    c = new_r_Const(current_ir_graph, block, mode, mag.M);
    q = new_rd_Mulh(dbg, current_ir_graph, block, n, c, mode);

    /* do we need an Add or Sub */
    if (mag.need_add)
      q = new_rd_Add(dbg, current_ir_graph, block, q, n, mode);
    else if (mag.need_sub)
      q = new_rd_Sub(dbg, current_ir_graph, block, q, n, mode);

    /* Do we need the shift */
    if (mag.s > 0) {
      c = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(mag.s, mode_Iu));
      q    = new_rd_Shrs(dbg, current_ir_graph, block, q, c, mode);
    }

    /* final */
    c = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(bits-1, mode_Iu));
    t = new_rd_Shr(dbg, current_ir_graph, block, q, c, mode);

    q = new_rd_Add(dbg, current_ir_graph, block, q, t, mode);
  }
  else {
    struct mu mag = magicu(tv);
    ir_node *c;

    /* generate the Mulh instruction */
    c = new_r_Const(current_ir_graph, block, mode, mag.M);
    q    = new_rd_Mulh(dbg, current_ir_graph, block, n, c, mode);

    if (mag.need_add) {
      if (mag.s > 0) {
        /* use the GM scheme */
        t = new_rd_Sub(dbg, current_ir_graph, block, n, q, mode);

        c = new_r_Const(current_ir_graph, block, mode_Iu, get_mode_one(mode_Iu));
        t = new_rd_Shr(dbg, current_ir_graph, block, t, c, mode);

        t = new_rd_Add(dbg, current_ir_graph, block, t, q, mode);

        c = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(mag.s-1, mode_Iu));
        q = new_rd_Shr(dbg, current_ir_graph, block, t, c, mode);
      }
      else {
        /* use the default scheme */
        q = new_rd_Add(dbg, current_ir_graph, block, q, n, mode);
      }
    }
    else if (mag.s > 0) { /* default scheme, shift needed */
      c = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(mag.s, mode_Iu));
      q = new_rd_Shr(dbg, current_ir_graph, block, q, c, mode);
    }
  }
  return q;
}

ir_node *arch_dep_replace_div_by_const(ir_node *irn)
{
  ir_node *res  = irn;

  /* If the architecture dependent optimizations were not initialized
     or this optimization was not enabled. */
  if (params == NULL || (opts & arch_dep_div_by_const) == 0)
    return irn;

  if (get_irn_opcode(irn) == iro_Div) {
    ir_node *c = get_Div_right(irn);
    ir_node *block, *left;
    ir_mode *mode;
    tarval *tv, *ntv;
    dbg_info *dbg;
    int n, bits;
    int k, n_flag;

    if (get_irn_op(c) != op_Const)
      return irn;

    left  = get_Div_left(irn);
    mode  = get_irn_mode(left);
    block = get_nodes_block(irn);
    dbg   = get_irn_dbg_info(irn);
    tv    = get_Const_tarval(c);

    bits = get_mode_size_bits(mode);
    n    = (bits + 7) / 8;

    k = -1;
    if (mode_is_signed(mode)) {
      /* for signed divisions, the algorithm works for a / -2^k by negating the result */
      ntv = tarval_neg(tv);
      n_flag = 1;
      k = tv_ld2(ntv, n);
    }

    if (k < 0) {
      n_flag = 0;
      k = tv_ld2(tv, n);
    }

    if (k >= 0) { /* division by 2^k or -2^k */
      if (mode_is_signed(mode)) {
        ir_node *k_node;
        ir_node *curr = left;

        if (k != 1) {
          k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k - 1, mode_Iu));
          curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
        }

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(bits - k, mode_Iu));
        curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

        curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k, mode_Iu));
        res    = new_rd_Shrs(dbg, current_ir_graph, block, curr, k_node, mode);

        if (n_flag) { /* negate the result */
          ir_node *k_node;

          k_node = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
          res = new_rd_Sub(dbg, current_ir_graph, block, k_node, res, mode);
        }
      }
      else {      /* unsigned case */
        ir_node *k_node;

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k, mode_Iu));
        res    = new_rd_Shr(dbg, current_ir_graph, block, left, k_node, mode);
      }
    }
    else {
      /* other constant */
      if ((mode_is_signed(mode) && params->allow_mulhs) ||
          (!mode_is_signed(mode) && params->allow_mulhu))
        res = replace_div_by_mulh(irn, tv);
    }
  }

  if (res != irn)
    stat_arch_dep_replace_div_by_const(irn);

  return res;
}

ir_node *arch_dep_replace_mod_by_const(ir_node *irn)
{
  ir_node *res  = irn;

  /* If the architecture dependent optimizations were not initialized
     or this optimization was not enabled. */
  if (params == NULL || (opts & arch_dep_mod_by_const) == 0)
    return irn;

  if (get_irn_opcode(irn) == iro_Mod) {
    ir_node *c = get_Mod_right(irn);
    ir_node *block, *left;
    ir_mode *mode;
    tarval *tv, *ntv;
    dbg_info *dbg;
    int n, bits;
    int k;

    if (get_irn_op(c) != op_Const)
      return irn;

    left  = get_Mod_left(irn);
    mode  = get_irn_mode(left);
    block = get_nodes_block(irn);
    dbg   = get_irn_dbg_info(irn);
    tv    = get_Const_tarval(c);

    bits = get_mode_size_bits(mode);
    n    = (bits + 7) / 8;

    k = -1;
    if (mode_is_signed(mode)) {
      /* for signed divisions, the algorithm works for a / -2^k by negating the result */
      ntv = tarval_neg(tv);
      k = tv_ld2(ntv, n);
    }

    if (k < 0) {
      k = tv_ld2(tv, n);
    }

    if (k >= 0) {
      /* division by 2^k or -2^k:
       * we use "modulus" here, so x % y == x % -y that's why is no difference between the case 2^k and -2^k
       */
      if (mode_is_signed(mode)) {
        ir_node *k_node;
        ir_node *curr = left;

        if (k != 1) {
          k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k - 1, mode_Iu));
          curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
        }

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(bits - k, mode_Iu));
        curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

        curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

        k_node = new_r_Const(current_ir_graph, block, mode, new_tarval_from_long((-1) << k, mode));
        curr   = new_rd_And(dbg, current_ir_graph, block, curr, k_node, mode);

        res    = new_rd_Sub(dbg, current_ir_graph, block, left, curr, mode);
      }
      else {      /* unsigned case */
        ir_node *k_node;

        k_node = new_r_Const(current_ir_graph, block, mode, new_tarval_from_long((1 << k) - 1, mode));
        res    = new_rd_And(dbg, current_ir_graph, block, left, k_node, mode);
      }
    }
    else {
      /* other constant */
      if ((mode_is_signed(mode) && params->allow_mulhs) ||
          (!mode_is_signed(mode) && params->allow_mulhu)) {
        res = replace_div_by_mulh(irn, tv);

        res = new_rd_Mul(dbg, current_ir_graph, block, res, c, mode);

        /* res = arch_dep_mul_to_shift(res); */

        res = new_rd_Sub(dbg, current_ir_graph, block, left, res, mode);
      }
    }
  }

  if (res != irn)
    stat_arch_dep_replace_mod_by_const(irn);

  return res;
}

void arch_dep_replace_divmod_by_const(ir_node **div, ir_node **mod, ir_node *irn)
{
  *div = *mod = NULL;

  /* If the architecture dependent optimizations were not initialized
     or this optimization was not enabled. */
  if (params == NULL ||
      ((opts & (arch_dep_div_by_const|arch_dep_mod_by_const)) != (arch_dep_div_by_const|arch_dep_mod_by_const)))
    return;

  if (get_irn_opcode(irn) == iro_DivMod) {
    ir_node *c = get_DivMod_right(irn);
    ir_node *block, *left;
    ir_mode *mode;
    tarval *tv, *ntv;
    dbg_info *dbg;
    int n, bits;
    int k, n_flag;

    if (get_irn_op(c) != op_Const)
      return;

    left  = get_DivMod_left(irn);
    mode  = get_irn_mode(left);
    block = get_nodes_block(irn);
    dbg   = get_irn_dbg_info(irn);
    tv    = get_Const_tarval(c);

    bits = get_mode_size_bits(mode);
    n    = (bits + 7) / 8;

    k = -1;
    if (mode_is_signed(mode)) {
      /* for signed divisions, the algorithm works for a / -2^k by negating the result */
      ntv = tarval_neg(tv);
      n_flag = 1;
      k = tv_ld2(ntv, n);
    }

    if (k < 0) {
      n_flag = 0;
      k = tv_ld2(tv, n);
    }

    if (k >= 0) { /* division by 2^k or -2^k */
      if (mode_is_signed(mode)) {
        ir_node *k_node, *c_k;
        ir_node *curr = left;

        if (k != 1) {
          k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k - 1, mode_Iu));
          curr   = new_rd_Shrs(dbg, current_ir_graph, block, left, k_node, mode);
        }

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(bits - k, mode_Iu));
        curr   = new_rd_Shr(dbg, current_ir_graph, block, curr, k_node, mode);

        curr   = new_rd_Add(dbg, current_ir_graph, block, left, curr, mode);

        c_k    = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k, mode_Iu));

        *div   = new_rd_Shrs(dbg, current_ir_graph, block, curr, c_k, mode);

        if (n_flag) { /* negate the div result */
          ir_node *k_node;

          k_node = new_r_Const(current_ir_graph, block, mode, get_mode_null(mode));
          *div = new_rd_Sub(dbg, current_ir_graph, block, k_node, *div, mode);
        }

        k_node = new_r_Const(current_ir_graph, block, mode, new_tarval_from_long((-1) << k, mode));
        curr   = new_rd_And(dbg, current_ir_graph, block, curr, k_node, mode);

        *mod   = new_rd_Sub(dbg, current_ir_graph, block, left, curr, mode);
      }
      else {      /* unsigned case */
        ir_node *k_node;

        k_node = new_r_Const(current_ir_graph, block, mode_Iu, new_tarval_from_long(k, mode_Iu));
        *div   = new_rd_Shr(dbg, current_ir_graph, block, left, k_node, mode);

        k_node = new_r_Const(current_ir_graph, block, mode, new_tarval_from_long((1 << k) - 1, mode));
        *mod   = new_rd_And(dbg, current_ir_graph, block, left, k_node, mode);
      }
    }
    else {
      /* other constant */
      if ((mode_is_signed(mode) && params->allow_mulhs) ||
          (!mode_is_signed(mode) && params->allow_mulhu)) {
        ir_node *t;

        *div = replace_div_by_mulh(irn, tv);

        t    = new_rd_Mul(dbg, current_ir_graph, block, *div, c, mode);

        /* t = arch_dep_mul_to_shift(t); */

        *mod = new_rd_Sub(dbg, current_ir_graph, block, left, t, mode);
      }
    }
  }

  if (*div)
    stat_arch_dep_replace_DivMod_by_const(irn);
}


static const arch_dep_params_t default_params = {
  1,  /* also use subs */
  4,  /* maximum shifts */
  31, /* maximum shift amount */

  0,  /* allow Mulhs */
  0,  /* allow Mulus */
  32  /* Mulh allowed up to 32 bit */
};

const arch_dep_params_t *arch_dep_default_factory(void) {
  return &default_params;
}
