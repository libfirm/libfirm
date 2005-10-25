#include <libfirm/firm.h>

#include "../firm2arch_nodes_attr.h"
#include "transform.h"
#include "new_nodes.h"

void transform_node(ir_node *node, void *env) {
  ir_mode *mode     = get_irn_mode(node);
  opcode  code      = get_irn_opcode(node);
  ir_node *op1      = NULL;
  ir_node *op2      = NULL;
  ir_node *asm_op   = NULL;
  ir_node *const_op = NULL;
  ir_node *expr_op  = NULL;
  opcode code_op1, code_op2;

  /* TODO: maybe one can transform
           "Const -- Sub -- Exp" into "Exp -- Minus -- Add -- Const"
           to save a register
           but I'm not sure about side effects */

#define GEN(a)  case iro_##a: asm_op = new_ia32_##a(op1, op2, mode); break
#define GENI(a)  case iro_##a: asm_op = new_ia32_##a##_i(expr_op, mode); break

  switch (code) {
    /* commutative arith ops */
    case iro_Add:
    case iro_Mul:
    case iro_And:
    case iro_Or:
    case iro_Eor:
      op1 = get_irn_n(node, 0);
      op2 = get_irn_n(node, 1);
      code_op1 = get_irn_opcode(op1);
      code_op2 = get_irn_opcode(op2);

      /* determine if one operator is a Const */
      const_op = code_op1 == iro_Const ? op1 : (code_op2 == iro_Const ? op2 : NULL);
      expr_op  = code_op1 != iro_Const ? op1 : (code_op2 != iro_Const ? op2 : NULL);

      goto gen_asm_node;

    /* not commutative arith ops: only construct the immediate nodes
       when the right operand is a Const */
    case iro_Sub:
    case iro_DivMod:
    case iro_Mod:
    case iro_Shl:
    case iro_Shr:
    case iro_Shrs:
    case iro_Rot:
      op1 = get_irn_n(node, 0);
      op2 = get_irn_n(node, 1);
      code_op1 = get_irn_opcode(op1);
      code_op2 = get_irn_opcode(op2);

      /* determine if the right operator is a Const */
      const_op = code_op2 == iro_Const ? op2 : NULL;
      expr_op  = op1;

gen_asm_node:
      assert(code_op1 != iro_Const && code_op2 != iro_Const && "Op(Const, Const) not expected!");
      if ((code_op1 == iro_Const) && (code_op2 == iro_Const)) {
        printf("Ignoring %s(Const, Const)\n", get_irn_opname(node));
        return;
      }

      if (const_op) { /* one of the operators is a Const */
        switch (code) {
          GENI(Add);
          GENI(Mul);
          GENI(And);
          GENI(Or);
          GENI(Eor);
          GENI(Sub);
//          GENI(DivMod);
          GENI(Mod);
          GENI(Shl);
          GENI(Shr);
          GENI(Shrs);
          GENI(Rot);
          default:
            /* ignore */
            break;
        }
        /* make the Const an attribute */
        asmop_attr *attr = (asmop_attr *)get_irn_generic_attr(const_op);
        attr->tv = get_Const_tarval(const_op);
      }
      else { /* both operators need to be computed */
        switch (code) {
          GEN(Add);
          GEN(Mul);
          GEN(And);
          GEN(Or);
          GEN(Eor);
          GEN(Sub);
//          GEN(DivMod);
          GEN(Mod);
          GEN(Shl);
          GEN(Shr);
          GEN(Shrs);
          GEN(Rot);
          default:
            /* ignore */
            break;
        }
      }

      /* exchange the old firm node with the new assembler node */
      exchange(node, asm_op);
      break;
    default:
      fprintf(stderr, "Ignoring node: %s\n", get_irn_opname(node));
      break;
  }
}
