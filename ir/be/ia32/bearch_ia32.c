#include "pseudo_irg.h"
#include "irgwalk.h"

#include "bitset.h"
#include "debug.h"

#include "../bearch.h"                /* the general register allocator interface */

#include "ia32_new_nodes.h"           /* ia32 nodes interface */
#include "gen_ia32_regalloc_if.h"     /* the generated interface (register type and class defenitions) */
#include "ia32_gen_decls.h"           /* interface declaration emitter */
#include "ia32_transform.h"

/* define shorter names for classes and indicees */

#define N_GP_REGS   N_ia32_general_purpose_REGS
#define N_FP_REGS   N_ia32_floating_point_REGS
#define N_FLAG_REGS N_ia32_flag_register_REGS

#define CLS_GP   CLASS_ia32_general_purpose
#define CLS_FP   CLASS_ia32_floating_point
#define CLS_FLAG CLASS_ia32_flag_register

#define N_CLASSES (sizeof(ia32_reg_classes) / sizeof(ia32_reg_classes[0]))

extern arch_register_class_t ia32_reg_classes[3];

/* Implementation of the register allocator functions */

/**
 * Return register requirements for an ia32 node.
 * If the node returns a tuple (mode_T) then the proj's
 * will be asked for this information.
 */
static const arch_register_req_t *ia32_get_irn_reg_req(const arch_irn_ops_t *self, arch_register_req_t *req, const ir_node *irn, int pos) {
  const arch_register_req_t **irn_req;

  if (is_Proj(irn)) {
  }
  else if (is_ia32_irn(irn)) {
    if (get_irn_mode(irn) == mode_T) {
      return NULL;
    }

    if (pos >= 0) {
      irn_req = get_ia32_in_req(irn);
    }
    else {
      irn_req = get_ia32_out_req(irn);
      pos     = -1 - pos;
    }

    memcpy(req, irn_req[pos], sizeof(*req));
    return req;
  }
  else
    req = NULL;

  return req;
}

static int ia32_get_n_operands(const arch_irn_ops_t *self, const ir_node *irn, int in_out) {
  if (in_out >= 0)
    return get_irn_arity(irn);
  else
    return get_ia32_n_res(irn);
}

static void ia32_set_irn_reg(const arch_irn_ops_t *self, ir_node *irn, int pos, const arch_register_t *reg) {
  if (is_ia32_irn(irn)) {
    const arch_register_t **slots;

    slots      = get_ia32_slots(irn);
    slots[pos] = reg;
  }
}

static const arch_register_t *ia32_get_irn_reg(const arch_irn_ops_t *self, const ir_node *irn, int pos) {
  if (is_ia32_irn(irn)) {
    const arch_register_t **slots;

    slots = get_ia32_slots(irn);

    return slots[pos];
  }
  else
    return NULL;
}

static arch_irn_class_t ia32_classify(const arch_irn_ops_t *self, const ir_node *irn) {
  if (is_ia32_irn(irn)) {
    if (is_ia32_Cmp(irn) || is_ia32_Cmp_i(irn)) // TODO: ia32_Jmp
      return arch_irn_class_branch;
    else
      return arch_irn_class_normal;
  }
  else
    return 0;
}

static arch_irn_flags_t ia32_get_flags(const arch_irn_ops_t *self, const ir_node *irn) {
  if (is_ia32_irn(irn))
    return get_ia32_flags(irn);
  else
    return 0;
}

/* fill register allocator interface */

static const arch_irn_ops_t ia32_irn_ops = {
  ia32_get_irn_reg_req,
  ia32_get_n_operands,
  ia32_set_irn_reg,
  ia32_get_irn_reg,
  ia32_classify,
  ia32_get_flags
};

/* Implementation of the backend isa functions */

static void ia32_init(void) {
  ia32_register_init();
  ia32_create_opcodes();
}

static int ia32_get_n_reg_class(void) {
  return N_CLASSES;
}

static const arch_register_class_t *ia32_get_reg_class(int i) {
  assert(i >= 0 && i < N_CLASSES && "Invalid ia32 register class requested.");
  return &ia32_reg_classes[i];
}

static const arch_irn_ops_t *ia32_get_irn_ops(const arch_irn_handler_t *self, const ir_node *irn) {
  return &ia32_irn_ops;
}

static void ia32_prepare_graph(ir_graph *irg) {
  firm_dbg_module_t *dbg = firm_dbg_register("be.transform.ia32");
  if (! is_pseudo_ir_graph(irg))
    irg_walk_blkwise_graph(irg, NULL, ia32_transform_node, dbg);
}

static void ia32_codegen(FILE *out) {
  ia32_gen_decls(out);

#if 0
  for (i = 0; i < get_irp_n_irgs(); ++i) {
    ir_graph *irg = get_irp_irg(i);
    if (! is_pseudo_ir_graph(irg)) {
      ia32_finish_irg(irg);
      ia32_gen_routine(out, irg);
    }
  }
#endif
}

const arch_irn_handler_t ia32_irn_handler = {
  ia32_get_irn_ops
};

/* fill isa interface */

const arch_isa_if_t ia32_isa = {
  ia32_init,
  ia32_get_n_reg_class,
  ia32_get_reg_class,
  ia32_prepare_graph,
  &ia32_irn_handler,
  ia32_codegen
};
