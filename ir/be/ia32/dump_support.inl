static void fprintf_tv(FILE *F, tarval *tv, int brackets) {
  char buf[1024];
  tarval_snprintf(buf, sizeof(buf), tv);

  if (brackets)
    fprintf(F, "[%s]", buf);
  else
    fprintf(F, "%s", buf);
}

const char *get_sc_name(ir_node *symc) {
  if (get_irn_opcode(symc) != iro_SymConst)
    return "NONE";

  switch (get_SymConst_kind(symc)) {
    case symconst_addr_name:
      return get_id_str(get_SymConst_name(symc));

    case symconst_addr_ent:
      return get_entity_ld_name(get_SymConst_entity(symc));

    default:
      assert(!"Unsupported SymConst");
  }
}

static int dump_node_ia32(ir_node *n, FILE *F, dump_reason_t reason) {
  const char *name, *p;
  ir_mode    *mode = NULL;
  int        bad   = 0;

  switch (reason) {
    case dump_node_opcode_txt:
      name = get_irn_opname(n);
      fprintf(F, "%s", name);
      break;

    case dump_node_mode_txt:
      mode = get_irn_mode(n);

      if (mode == mode_BB || mode == mode_ANY || mode == mode_BAD || mode == mode_T) {
        mode = NULL;
      }
      else if (is_ia32_Load(n)) {
        mode = get_irn_mode(get_irn_n(n, 1));
      }
      else if (is_ia32_Store(n)) {
        mode = get_irn_mode(get_irn_n(n, 2));
      }

      if (mode)
        fprintf(F, "[%s]", get_mode_name(mode));
      break;

    case dump_node_nodeattr_txt:
      name = get_irn_opname(n);
      p = name + strlen(name) - 2;
      if (p[0] == '_' && p[1] == 'i') {
        tarval *tv = get_Immop_tarval(n);
        if (tv)
          fprintf_tv(F, tv, 1);
        else {
          fprintf(F, "[SymC &%s]", get_sc_name(get_old_ir(n)));
        }
      }
      else if (is_ia32_Call(n)) {
        ir_node *old_call = get_old_ir(n);
        ir_node *old_imm  = get_Call_ptr(old_call);

        fprintf(F, "&%s ", get_sc_name(get_Imm_sc(old_imm)));
      }
      break;

    case dump_node_info_txt:
      if (is_ia32_Lea(n)) {
        tarval *o  = get_ia32_Lea_offs(n);
        tarval *tv = get_Immop_tarval(n);

        fprintf(F, "LEA ");
        if (o)
          fprintf_tv(F, o, 0);
        fprintf(F, "(%s, %s", get_irn_opname(get_irn_n(n, 0)), get_irn_opname(get_irn_n(n, 1)));
        if (tv) {
          fprintf(F, ", ");
          fprintf_tv(F, tv, 0);
        }
        fprintf(F, ")\n");
      }
      break;
  }

  return bad;
}
