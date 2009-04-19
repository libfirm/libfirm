/*
 * Firm - Evaluator
 *
 * (C) 2005 Michael Beck    beck@ipd.info.uni-karlsruhe.de
 */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tchar.h>
#include <malloc.h>
#include <stdarg.h>

#include "config.h"

/* ugly, but I must include array.h WITHOUT NDEBUG */
#ifdef NDEBUG
#undef NDEBUG
#include "array_t.h"
#define NDEBUG
#else
#include "array.h"
#endif

#include "entity_t.h"
#include "irgraph_t.h"
#include "irnode_t.h"
#include "irmode_t.h"
#include "tv_t.h"
#include "irloop_t.h"
#include "irextbb_t.h"
#include "irprog_t.h"
#include "tpop_t.h"
#include "strcalc.h"
#include "fltcalc.h"

#include "set.h"
#include "pset.h"
#include "pdeq.h"
#include "bitset.h"

#include "firmEvaluator.h"

/** get the address of a pointer */
#define ADD_ADR(p, off)  ((void *)((char *)(p) + (off)))

/** debug output */
static void debug(char *fmt, ...) {
  va_list ap;
  char buf[1024];

  va_start(ap, fmt);
  vsprintf(buf, fmt, ap);

  OutputDebugString(buf);
  va_end(ap);
}  /* debug */

/**
 * return the size of a firm object
 */
int get_firm_object_size(firm_kind kind) {
  switch (kind) {
  case k_entity:     /* an entity */
    return sizeof(ir_entity);
  case k_type:       /* a type */
    return sizeof(ir_type);
  case k_ir_graph:   /* an ir graph */
    return sizeof(ir_graph);
  case k_ir_node:    /* an ir node */
    return sizeof(ir_node);
  case k_ir_mode:    /* an ir mode */
    return sizeof(ir_mode);
  case k_ir_op:      /* an ir opcode */
    return sizeof(ir_op);
  case k_tarval:     /* a tarval */
    return sizeof(tarval);
  case k_ir_loop:    /* a loop */
    return sizeof(ir_loop);
  case k_ir_compound_graph_path: /* a compound graph path, see entity.h */
    return sizeof(compound_graph_path);
  case k_ir_extblk:  /* an extended block */
    return sizeof(ir_extblk);
  case k_ir_prog:    /* a program representation (irp) */
    return sizeof(ir_prog);
  default:
    return 0;
  }
}  /* get_firm_object_size */

/**
 * returns the string length of a string in debuggee space
 *
 * @return string length or negative value on error
 */
static int strlen_debuggee(DEBUGHELPER *pHelper, const void *address, size_t max)
{
  size_t i;
  char v;
  const char *p = address;

  for (i = 0; i < max; ++i) {
    if (copy_from_debuggee(p + i, pHelper, &v, sizeof(v)) != S_OK)
      return -1;

    if (! v)
      return i;
  }
  return i;
}  /* strlen_debuggee */

/**
 * Format an ident
 */
HRESULT format_ident(DEBUGHELPER *pHelper, const void *address, char *pResult, size_t max) {
  set_entry *data = NULL;
  set_entry id;
  size_t len, slen;

  if (copy_from_debuggee(address, pHelper, &id, sizeof(id)) != S_OK)
    return E_FAIL;

  /* safety check */
  if (id.size < 1 || id.size > 256)
    return E_FAIL;

  slen = id.size + 1;
  len = offsetof(set_entry, dptr) + slen;

  data = alloca(len);

  if (copy_from_debuggee(address, pHelper, data, len) != S_OK)
    return E_FAIL;

  _tcsncpy(pResult, (const char *)data->dptr, max);
  return S_OK;
}  /* format_ident */

/**
 * Format a tp_op
 */
static HRESULT format_tp_op(DEBUGHELPER *pHelper, const void *addr, char *pResult, size_t max)
{
  tp_op op;

#define X(a)     case tpo_##a: _tcsncpy(pResult, #a, max); return S_OK
#define Y(a, b)  case tpo_##a: _tcsncpy(pResult, b, max); return S_OK

  if (copy_from_debuggee(addr, pHelper, &op, sizeof(op)) != S_OK)
    return E_FAIL;

  switch (op.code) {
  X(uninitialized);
  X(class);
  X(struct);
  X(method);
  X(union);
  X(array);
  Y(enumeration, "enum");
  Y(pointer, "ptr");
  Y(primitive, "prim");
  X(id);
  X(none);
  X(unknown);
  default:
    return E_FAIL;
  }
#undef X
#undef Y
}  /* format_tp_op */

/**
 * Checks whether a type is the global type
 *
 * @param type  the address of the type in debuggee's space
 */
static HRESULT is_global_type(DEBUGHELPER *pHelper, const void *type, int *flag) {
  ir_type tp;

  *flag = 0;
  if (copy_from_debuggee(type, pHelper, &tp, sizeof(tp)) != S_OK)
    return E_FAIL;

  *flag = tp.flags & tf_global_type;
  return S_OK;
}  /* is_global_type */

/**
 * format an entity
 */
static HRESULT format_entity(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max, int top) {
  ir_entity ent;
  ir_type owner;
  char name[256];
  int is_global;

  if (copy_from_debuggee(addr, pHelper, &ent, sizeof(ent)) != S_OK) {
    return E_FAIL;
  }
  if (is_global_type(pHelper, ent.owner, &is_global) != S_OK)
    return E_FAIL;

  *pResult = '\0';
  if (top)
    _tcsncpy(pResult, "ENT: ", max);

  if (! is_global) {
    if (copy_from_debuggee(ent.owner, pHelper, &owner, sizeof(owner)) != S_OK)
      return E_FAIL;
    if (format_ident(pHelper, (void *)owner.name, name, sizeof(name)) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, "::", max);
  }

  if (format_ident(pHelper, (void *)ent.name, name, sizeof(name)) != S_OK)
    return E_FAIL;
  _tcsncat(pResult, name, max);

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), " [0x%lx]", ent.nr);
    break;
  case 8:
    _snprintf(name, sizeof(name), " [0%lo]", ent.nr);
    break;
  default:
    _snprintf(name, sizeof(name), " [%ld]", ent.nr);
  }
  _tcsncat(pResult, name, max);

  return S_OK;
}  /* format_entity */

/**
 * format a type
 */
static HRESULT format_type(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max, int top) {
  ir_type tp;
  char name[256];

  if (copy_from_debuggee(addr, pHelper, &tp, sizeof(tp)) != S_OK)
    return E_FAIL;

  pResult[0] = '\0';
  if (top) {
    if (format_tp_op(pHelper, tp.type_op, pResult, max) != S_OK)
      return E_FAIL;

    _tcsncat(pResult, " ", max);
  }

  if (format_ident(pHelper, tp.name, name, sizeof(name)) != S_OK)
    return E_FAIL;

  _tcsncat(pResult, name, max);
  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), " [0x%lx]", tp.nr);
    break;
  case 8:
    _snprintf(name, sizeof(name), " [0%lo]", tp.nr);
    break;
  default:
    _snprintf(name, sizeof(name), " [%ld]", tp.nr);
  }
  _tcsncat(pResult, name, max);

  return S_OK;
}  /* format_type */

/**
 * format an irg
 */
static HRESULT format_irg(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max, int top) {
  ir_graph irg;
  char name[256];

  if (copy_from_debuggee(addr, pHelper, &irg, sizeof(irg)) != S_OK)
    return E_FAIL;

  *pResult = '\0';
  if (top)
    _tcsncpy(pResult, "IRG: ", max);

  if (irg.ent) {
    ir_entity ent;
    ir_type owner;
    int is_global;

    if (copy_from_debuggee(irg.ent, pHelper, &ent, sizeof(ent)) != S_OK)
      return E_FAIL;
    if (is_global_type(pHelper, ent.owner, &is_global) != S_OK)
      return E_FAIL;
    if (! is_global) {
      if (copy_from_debuggee(ent.owner, pHelper, &owner, sizeof(owner)) != S_OK)
        return E_FAIL;
      if (format_ident(pHelper, (void *)owner.name, name, sizeof(name)) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      _tcsncat(pResult, "::", max);
    }
    if (format_ident(pHelper, ent.name, name, sizeof(name)) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
  }
  else
    _tcsncat(pResult, "NULL", max);

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), " [0x%lx, 0x%u nodes]", irg.graph_nr, irg.last_node_idx);
    break;
  case 8:
    _snprintf(name, sizeof(name), " [0%lo, 0%o nodes]", irg.graph_nr, irg.last_node_idx);
    break;
  default:
    _snprintf(name, sizeof(name), " [%ld, %u nodes]", irg.graph_nr, irg.last_node_idx);
  }
  _tcsncat(pResult, name, max);
  return S_OK;
}  /* format_irg */

/**
 * format an ir_op
 */
HRESULT format_op(DEBUGHELPER *pHelper, const void *addr, char *pResult, size_t max) {
  ir_op op;

  if (copy_from_debuggee(addr, pHelper, &op, sizeof(op)) != S_OK)
    return E_FAIL;
  if (format_ident(pHelper, op.name, pResult, max) != S_OK)
    return E_FAIL;
  return S_OK;
}  /* format_op */

/**
 * format an ir_mode
 */
static HRESULT format_mode(DEBUGHELPER *pHelper, const void *addr, char *pResult, size_t max) {
  ir_mode mode;

  if (copy_from_debuggee(addr, pHelper, &mode, sizeof(mode)) != S_OK)
    return E_FAIL;
  if (format_ident(pHelper, mode.name, pResult, max) != S_OK)
    return E_FAIL;
  return S_OK;
}  /* format_mode */

/** get a temporary string */
#define get_string(str)                                         \
do {                                                            \
  int len;                                                      \
  char *s;                                                      \
  if (str) {                                                    \
    len = strlen_debuggee(pHelper, str, 256);                   \
    if (len < 0)                                                \
      return E_FAIL;                                            \
    s = alloca(len + 1);                                        \
    if (copy_from_debuggee(str, pHelper, s, (DWORD)len) != S_OK) \
      return E_FAIL;                                            \
    s[len] = '\0';                                              \
    str = s;                                                    \
  }                                                             \
} while(0)

/**
 * format a tarval
 */
static HRESULT format_tarval(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max)
{
  tarval tv;
  char *value;
  ir_mode mode;
  unsigned len;
  tarval_mode_info modinfo;

  {
    static int initialized = 0;

    if (! initialized) {
      /* from init_tarval_1() */
      init_strcalc(68);
      init_fltcalc(0);

      initialized = 1;
    }
  }

  if (copy_from_debuggee(addr, pHelper, &tv, sizeof(tv)) != S_OK)
    return E_FAIL;

  /* ir_mode */
  if (tv.mode == NULL)
    return E_FAIL;

  if (copy_from_debuggee(tv.mode, pHelper, &mode, sizeof(mode)) != S_OK)
    return E_FAIL;

  tv.mode = &mode;

  if (mode_is_int(&mode)) {
    switch (nBase) {
    case 16:
      modinfo.mode_output = TVO_HEX;
      modinfo.mode_prefix = "0x";
      modinfo.mode_suffix = "";
      break;
    case 8:
      modinfo.mode_output = TVO_OCTAL;
      modinfo.mode_prefix = "0";
      modinfo.mode_suffix = "";
      break;
    default:
      modinfo.mode_output = TVO_DECIMAL;
      modinfo.mode_prefix = "";
      modinfo.mode_suffix = "";
    }
  }
  else {
    if (mode.tv_priv) {
      if (copy_from_debuggee(mode.tv_priv, pHelper, &modinfo, sizeof(modinfo)) != S_OK)
        return E_FAIL;

      get_string(modinfo.mode_prefix);
      get_string(modinfo.mode_suffix);
    }
  }
  mode.tv_priv = &modinfo;

  len = tv.length;
  if (len) {
    if (len > 256)
      return E_FAIL;

    value = alloca(len);

    if (copy_from_debuggee(tv.value, pHelper, value, len) != S_OK)
      return E_FAIL;

    tv.value = value;

    tarval_snprintf(pResult, max, &tv);
  }
  else {
    /* might be a reserved tarval */
    int resid = PTR_TO_INT(tv.value);

    switch (resid) {
    case resid_tarval_bad:
      _tcsncat(pResult, "BAD", max);
      break;
    case resid_tarval_undefined:
      _tcsncat(pResult, "UNDEF", max);
      break;
    case resid_tarval_b_false:
      _tcsncat(pResult, "FALSE", max);
      break;
    case resid_tarval_b_true:
      _tcsncat(pResult, "TRUE", max);
      break;
    default:
      /* try it */
      tarval_snprintf(pResult, max, &tv);
    }
  }
  return S_OK;
}  /* format_tarval */

/**
 * format an ir_node
 */
static HRESULT format_node(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max, int top) {
  ir_node n;
  char name[256];
  ir_op op;

  if (copy_from_debuggee(addr, pHelper, &n, sizeof(n)) != S_OK)
    return E_FAIL;

  /* ir_op */
  if (format_op(pHelper, n.op, pResult, max) != S_OK)
    return E_FAIL;

  /* ir_mode */
  if (format_mode(pHelper, n.mode, name, sizeof(name)) != S_OK)
    return E_FAIL;
  _tcsncat(pResult, name, max);

  if (copy_from_debuggee(n.op, pHelper, &op, sizeof(op)) != S_OK)
    return E_FAIL;

  /* show show node attributes */
  switch (op.code) {
  case iro_Const:
    if (format_tarval(pHelper, nBase, n.attr.con.tv, name, sizeof(name)) != S_OK) {
      _tcsncat(pResult, "<???>", max);
    }
    else {
      _tcsncat(pResult, "<", max);
      _tcsncat(pResult, name, max);
      _tcsncat(pResult, ">", max);
    }
    break;
  case iro_SymConst:
    _tcsncat(pResult, "<", max);
    switch (n.attr.symc.kind) {
    case symconst_type_tag:
      _tcsncat(pResult, "TAG:", max);
      if (format_type(pHelper, nBase, n.attr.symc.sym.type_p, name, sizeof(name), 0) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      break;
    case symconst_type_size:
      _tcsncat(pResult, "SIZE:", max);
      if (format_type(pHelper, nBase, n.attr.symc.sym.type_p, name, sizeof(name), 0) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      break;
    case symconst_type_align:
      _tcsncat(pResult, "ALGN:", max);
      if (format_type(pHelper, nBase, n.attr.symc.sym.type_p, name, sizeof(name), 0) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      break;
    case symconst_addr_name:
      _tcsncat(pResult, "NAME:", max);
      if (format_ident(pHelper, n.attr.symc.sym.ident_p, name, sizeof(name)) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      break;
    case symconst_addr_ent:
      _tcsncat(pResult, "ENT:", max);
      if (format_entity(pHelper, nBase, n.attr.symc.sym.entity_p, name, sizeof(name), 0) != S_OK)
        return E_FAIL;
      _tcsncat(pResult, name, max);
      break;
    }
    _tcsncat(pResult, ">", max);
    break;
  case iro_Sel:
    _tcsncat(pResult, "<", max);
    if (format_entity(pHelper, nBase, n.attr.sel.entity, name, sizeof(name), 0) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, ">", max);
    break;
  case iro_Cast:
    _tcsncat(pResult, "<", max);
    if (format_type(pHelper, nBase, n.attr.cast.type, name, sizeof(name), 0) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, ">", max);
    break;
  case iro_Alloc:
    _tcsncat(pResult, "<", max);
    if (format_type(pHelper, nBase, n.attr.alloc.type, name, sizeof(name), 0) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _snprintf(name, sizeof(name), ", %s", n.attr.alloc.where == stack_alloc ? "stack" : "heap");
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, ">", max);
    break;
  case iro_Free:
    _tcsncat(pResult, "<", max);
    if (format_type(pHelper, nBase, n.attr.free.type, name, sizeof(name), 0) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _snprintf(name, sizeof(name), ", %s", n.attr.free.where == stack_alloc ? "stack" : "heap");
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, ">", max);
    break;
  case iro_CopyB:
    _tcsncat(pResult, "<", max);
    if (format_type(pHelper, nBase, n.attr.copyb.type, name, sizeof(name), 0) != S_OK)
      return E_FAIL;
    _tcsncat(pResult, name, max);
    _tcsncat(pResult, ">", max);
    break;
  }

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), " [0x%lx:0x%x]", n.node_nr, n.node_idx);
    break;
  case 8:
    _snprintf(name, sizeof(name), " [0%lo:0%o]", n.node_nr, n.node_idx);
    break;
  default:
    _snprintf(name, sizeof(name), " [%ld:%u]", n.node_nr, n.node_idx);
  }
  _tcsncat(pResult, name, max);

  return S_OK;
}  /* format_node */

/**
 * format a loop
 */
static HRESULT format_loop(DEBUGHELPER *pHelper, const void *addr, char *pResult, size_t max)
{
  ir_loop loop;

  if (copy_from_debuggee(addr, pHelper, &loop, sizeof(loop)) != S_OK)
    return E_FAIL;
  return E_FAIL;
}  /* format_loop */

/**
 * Get an array descriptor
 */
static HRESULT get_array_desc(DEBUGHELPER *pHelper, const void *address, ir_arr_descr *desc)
{
  address = ARR_DESCR(address);
  if (copy_from_debuggee(address, pHelper, desc, sizeof(*desc)) != S_OK)
    return E_FAIL;

  return S_OK;
}  /* get_array_desc */

/**
 * format an extended block
 */
static HRESULT format_extblk(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max){
  ir_extblk extbb;
  ir_arr_descr blocks;
  ir_node *blks = NULL;
  char name[256];
  int len;

  if (copy_from_debuggee(addr, pHelper, &extbb, sizeof(extbb)) != S_OK)
    return E_FAIL;
  if (extbb.blks == NULL)
    return E_FAIL;

  if (get_array_desc(pHelper, extbb.blks, &blocks) != S_OK)
    return E_FAIL;

  len = ARR_LEN(&blocks.v.elts);

  if (len > 0) {
    if (copy_from_debuggee(extbb.blks, pHelper, &blks, sizeof(blks)) != S_OK)
      return E_FAIL;
  }

  if (blks) {
    switch (nBase) {
    case 16:
      _snprintf(name, sizeof(name), "0x%x 0x%x blocks", blks->node_nr, len);
      break;
    case 8:
      _snprintf(name, sizeof(name), "0%o 0%o blocks", blks->node_nr, len);
      break;
    default:
      _snprintf(name, sizeof(name), "%d %d blocks", blks->node_nr, len);
    }
    _tcsncpy(pResult, name, max);
  }
  else
    _tcsncpy(pResult, "<EMPTY>", max);
  return S_OK;
}  /* format_extblk */


/**
 * format a ir_prog
 */
static HRESULT format_prog(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max)
{
  ir_prog irp;
  ir_arr_descr graphs, types;
  char name[256];

  if (copy_from_debuggee(addr, pHelper, &irp, sizeof(irp)) != S_OK)
    return E_FAIL;
  if (irp.graphs) {
    if (get_array_desc(pHelper, irp.graphs, &graphs) != S_OK)
      return E_FAIL;

    irp.graphs = (ir_graph**)&graphs.v.elts;
  }

  if (irp.types) {
    if (get_array_desc(pHelper, irp.types, &types) != S_OK)
      return E_FAIL;

    irp.types = (ir_type**)&types.v.elts;
  }

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "0x%x graphs 0x%x types", ARR_LEN(irp.graphs), ARR_LEN(irp.types));
    break;
  case 8:
    _snprintf(name, sizeof(name), "0%o graphs 0%o types", ARR_LEN(irp.graphs), ARR_LEN(irp.types));
    break;
  default:
    _snprintf(name, sizeof(name), "%d graphs %d types", ARR_LEN(irp.graphs), ARR_LEN(irp.types));
  }
  _tcsncpy(pResult, name, max);

  return S_OK;
}  /* format_prog */

/*
 * Format an array descriptor
 */
HRESULT format_arr_descr(DEBUGHELPER *pHelper, int nBase, const void *addr, char *pResult, size_t max)
{
  ir_arr_descr desc;
  char name[256];

  if (copy_from_debuggee(addr, pHelper, &desc, sizeof(desc)) != S_OK)
    return E_FAIL;

  switch (desc.magic) {
  case ARR_D_MAGIC:
    _tcsncpy(pResult, "DynArr ", max); break;
  case ARR_A_MAGIC:
    _tcsncpy(pResult, "AutoArr ", max); break;
  case ARR_F_MAGIC:
    _tcsncpy(pResult, "FlexArr ", max); break;
  default:
    _tcsncpy(pResult, "UNKN ", max);
  }

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "nelts 0x%x", desc.nelts);
    break;
  case 8:
    _snprintf(name, sizeof(name), "nelts 0%o", desc.nelts);
    break;
  default:
    _snprintf(name, sizeof(name), "nelts %d", desc.nelts);
  }
  _tcsncat(pResult, name, max);

  return S_OK;
}  /* format_arr_descr */

/*
 * format a firm object
 */
HRESULT FormatFirmObject(DEBUGHELPER *pHelper, int nBase, firm_kind kind, const void *addr, char *pResult, size_t max)
{
  switch (kind) {
  case k_entity:     /* an entity */
    return format_entity(pHelper, nBase, addr, pResult, max, 1);
  case k_type:       /* a type */
    return format_type(pHelper, nBase, addr, pResult, max, 1);
  case k_ir_graph:   /* an ir graph */
    return format_irg(pHelper, nBase, addr, pResult, max, 1);
  case k_ir_node:    /* an ir node */
    return format_node(pHelper, nBase, addr, pResult, max, 1);
  case k_ir_mode:    /* an ir mode */
    return format_mode(pHelper, addr, pResult, max);
  case k_ir_op:      /* an ir opcode */
    return format_op(pHelper, addr, pResult, max);
  case k_tarval:     /* a tarval */
    return format_tarval(pHelper, nBase, addr, pResult, max);
  case k_ir_loop:    /* a loop */
    return format_loop(pHelper, addr, pResult, max);
  case k_ir_compound_graph_path: /* a compound graph path, see entity.h */
    return E_FAIL;
  case k_ir_extblk:  /* an extended block */
    return format_extblk(pHelper, nBase, addr, pResult, max);
  case k_ir_prog:    /* a program representation (irp) */
    return format_prog(pHelper, nBase, addr, pResult, max);
  default:
    return E_FAIL;
  }
}  /* FormatFirmObject */

#define SEGMENT_SIZE_SHIFT	8
#define SEGMENT_SIZE		(1 << SEGMENT_SIZE_SHIFT)
#define DIRECTORY_SIZE_SHIFT	8
#define DIRECTORY_SIZE		(1 << DIRECTORY_SIZE_SHIFT)
#define MAX_LOAD_FACTOR		4

typedef struct pset_element {
  struct pset_element *chain;	/**< for chaining Elements */
  pset_entry entry;
} pset_Element, *pset_Segment;

/* not visible from outside */
struct pset {
  unsigned p;              /**< Next bucket to be split */
  unsigned maxp;           /**< upper bound on p during expansion */
  unsigned nkey;           /**< current # keys */
  unsigned nseg;           /**< current # segments */
  pset_Segment *dir[DIRECTORY_SIZE];
  int (*cmp)();            /**< function comparing entries */
  unsigned iter_i, iter_j;
  pset_Element *iter_tail; /**< non-NULL while iterating over elts */
  pset_Element *free_list; /**< list of free Elements */
  struct obstack obst;     /**< obstack for allocation all data */
#ifdef STATS
  int naccess, ncollision, ndups;
  int max_chain_len;
#endif
#ifdef DEBUG
  const char *tag;         /**< an optionally tag for distinguishing sets */
#endif
};

typedef struct set_element {
  struct set_element *chain;	/**< for chaining Elements */
  set_entry entry;
} set_Element, *set_Segment;

/* not visible from outside */
struct set {
  unsigned p;              /**< Next bucket to be split */
  unsigned maxp;           /**< upper bound on p during expansion */
  unsigned nkey;           /**< current # keys */
  unsigned nseg;           /**< current # segments */
  set_Segment *dir[DIRECTORY_SIZE];
  int (*cmp)();            /**< function comparing entries */
  unsigned iter_i, iter_j;
  set_Element *iter_tail;  /**< non-NULL while iterating over elts */
  struct obstack obst;     /**< obstack for allocation all data */
#ifdef STATS
  int naccess, ncollision, ndups;
  int max_chain_len;
#endif
#ifdef DEBUG
  const char *tag;         /**< an optionally tag for distinguishing sets */
#endif
};

/**
 * Find the longest chain of a pset
 */
static HRESULT find_longest_pset_chain(DEBUGHELPER *pHelper, pset *set,
                                       int *chains, int *lenght, size_t *size) {
  unsigned i, j;
  pset_Segment *seg, *curr;
  pset_Element elem;
  void *address;
  int len, nchains = 0, max_len = 0;
  size_t dyns = 0;

  for (i = 0; i < set->nseg; ++i) {
    seg = set->dir[i];

    dyns += sizeof(seg[j]) * SEGMENT_SIZE;
    for (j = 0; j < SEGMENT_SIZE; ++j) {
      if (copy_from_debuggee(&seg[j], pHelper, &curr, sizeof(curr)) != S_OK)
        return E_FAIL;

      address = curr;
      if (address)
        ++nchains;
      for (len = 0; address != NULL; address = elem.chain) {
        if (copy_from_debuggee(address, pHelper, &elem, sizeof(elem)) != S_OK)
          return E_FAIL;
        dyns += sizeof(pset_Element);
        ++len;
      }
      if (len > max_len)
        max_len = len;
    }
  }

  *chains = nchains;
  *lenght = max_len;
  *size   = dyns;
  return S_OK;
}  /* find_longest_pset_chain */

/**
 * Find the longest chain of a set
 */
static HRESULT find_longest_set_chain(DEBUGHELPER *pHelper, set *set,
                                      int *chains, int *lenght, size_t *size) {
  unsigned i, j;
  set_Segment *seg, *curr;
  set_Element elem;
  void *address;
  int len, nchains = 0, max_len = 0;
  size_t dyns = 0;

  for (i = 0; i < set->nseg; ++i) {
    seg = set->dir[i];

    dyns += sizeof(seg[j]) * SEGMENT_SIZE;
    for (j = 0; j < SEGMENT_SIZE; ++j) {
      if (copy_from_debuggee(&seg[j], pHelper, &curr, sizeof(curr)) != S_OK)
          return E_FAIL;

      address = curr;
      if (address)
        ++nchains;
      for (len = 0; address != NULL; address = elem.chain) {
        if (copy_from_debuggee(address, pHelper, &elem, sizeof(elem)) != S_OK)
          return E_FAIL;
        dyns += offsetof(set_Element, entry.dptr) + elem.entry.size;
        ++len;
      }
      if (len > max_len)
        max_len = len;
    }
  }

  *chains = nchains;
  *lenght = max_len;
  *size   = dyns;
  return S_OK;
}  /* find_longest_set_chain */

/*
 * Format a pset
 */
HRESULT format_pset(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max)
{
  pset set;
  char name[256];
  int nchains, chain_len;
  size_t size;

  if (copy_from_debuggee(address, pHelper, &set, sizeof(set)) != S_OK)
    return E_FAIL;

  if (find_longest_pset_chain(pHelper, &set, &nchains, &chain_len, &size) != S_OK)
    return E_FAIL;

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "nkey 0x%x nseg 0x%x nchain 0x%x maxlen 0x%x size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
    break;
  case 8:
    _snprintf(name, sizeof(name), "nkey 0%o nseg 0%o nchain 0%o maxlen 0%o size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
    break;
  default:
    _snprintf(name, sizeof(name), "nkey %u nseg %d nchain %d maxlen %d size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
  }
  _tcsncpy(pResult, name, max);

  return S_OK;
}  /* format_pset */

/*
 * Format a set
 */
HRESULT format_set(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max)
{
  set set;
  char name[256];
  int nchains, chain_len;
  size_t size;

  if (copy_from_debuggee(address, pHelper, &set, sizeof(set)) != S_OK)
    return E_FAIL;

  if (find_longest_set_chain(pHelper, &set, &nchains, &chain_len, &size) != S_OK)
    return E_FAIL;

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "nkey 0x%x nseg 0x%x nchain 0x%x maxlen 0x%x size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
    break;
  case 8:
    _snprintf(name, sizeof(name), "nkey 0%o nseg 0%o nchain 0%o maxlen 0%o size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
    break;
  default:
    _snprintf(name, sizeof(name), "nkey %u nseg %d nchain %d maxlen %d size %u kB", set.nkey, set.nseg, nchains, chain_len, (size + 1023) >> 10);
  }
  _tcsncpy(pResult, name, max);

  return S_OK;
}  /* format_set */

struct pdeq {
  unsigned magic;       /**< debug magic, only available in DEBUG builds */
  pdeq *l_end, *r_end;  /**< left and right ends of the queue */
  pdeq *l, *r;          /**< left and right neighbor */
  int n;                /**< number of elements in the current chunk */
  int p;                /**< the read/write pointer */
  const void *data[1];  /**< storage for elements */
};

/** Returns the length of a double ended pointer list. */
static int get_pdeq_len(DEBUGHELPER *pHelper, pdeq *dq)
{
  int n;
  pdeq *q;
  pdeq pdeq;

  n = 0;
  q = dq->l_end;

  if (copy_from_debuggee(q, pHelper, &pdeq, sizeof(pdeq)) != S_OK)
    return -1;
  q = &pdeq;

  for (;;) {
    n += q->n;
    q = q->r;
    if (! q)
      break;

    if (copy_from_debuggee(q, pHelper, &pdeq, sizeof(pdeq)) != S_OK)
      return -1;
    q = &pdeq;
  };

  return n;
}  /* get_pdeq_len */

/*
 * Format a pdeq
 */
HRESULT format_pdeq(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max)
{
  pdeq pdeq;
  char name[256];
  int len;

  if (copy_from_debuggee(address, pHelper, &pdeq, sizeof(pdeq)) != S_OK)
    return E_FAIL;

  len = get_pdeq_len(pHelper, &pdeq);
  if (len < 0)
    return E_FAIL;

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "pdeq 0x%x elem", len);
    break;
  case 8:
    _snprintf(name, sizeof(name), "pdeq 0%o elem", len);
    break;
  default:
    _snprintf(name, sizeof(name), "pdeq %d elem", len);
  }
  _tcsncpy(pResult, name, max);

  return S_OK;
}  /* format_pdeq */

/** show the first 2 units */
static HRESULT fill_bits(DEBUGHELPER *pHelper, bitset_t *bs, char *pResult) {
  bitset_pos_t i, units = bs->units;
  int l = 0, o = 0, breaked = 0;
  unsigned j;

  for (i = 0; i < units; ++i) {
    bitset_unit_t data;

    if (copy_from_debuggee((void *)(BS_DATA(bs)[i]), pHelper, &data, sizeof(data)) != S_OK)
      return E_FAIL;

    for (j = 0; j < 32; ++j) {
      if (data & (1 << j)) {
        sprintf(pResult + l, "%d,", i * sizeof(data) * 8 + j);
        l += strlen(pResult + l);
        ++o;
        if (o >= 10) {
          breaked = 1;
          goto end;
        }
      }
    }
  }
end:
  if (breaked) {
    sprintf(pResult + l, "...");
    l += 3;
  }
  sprintf(pResult + l, "}");
  return S_OK;
}  /* fill_bits */

/*
 * Format a bitset
 */
HRESULT format_bitset(DEBUGHELPER *pHelper, int nBase, const void *address, char *pResult, size_t max)
{
  bitset_t bs;
  char name[256];
  bitset_pos_t l;

  if (copy_from_debuggee(address, pHelper, &bs, sizeof(bs)) != S_OK)
    return E_FAIL;

  switch (nBase) {
  case 16:
    _snprintf(name, sizeof(name), "bitset{0x%x:", bs.size);
    break;
  case 8:
    _snprintf(name, sizeof(name), "bitset{0%o:", bs.size);
    break;
  default:
    _snprintf(name, sizeof(name), "bitset{%u:", bs.size);
  }

  l = strlen(name);
  if (fill_bits(pHelper, &bs, &name[l]) != S_OK)
    return E_FAIL;


  _tcsncpy(pResult, name, max);

  return S_OK;
}  /* format_bitset */
