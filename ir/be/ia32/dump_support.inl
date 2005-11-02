static void fprintf_tv(FILE *F, tarval *tv, int brackets) {
  char buf[1024];
  tarval_snprintf(buf, sizeof(buf), tv);

  if (brackets)
    fprintf(F, "[%s]", buf);
  else
    fprintf(F, "%s", buf);
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

      if (mode && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T) {
        /* dump below */
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
          fprintf(F, "[SymConst]");
        }
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
