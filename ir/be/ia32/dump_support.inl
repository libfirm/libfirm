static int dump_node_ia32(ir_node *n, FILE *F, dump_reason_t reason)
{
  const char *name, *p;
  ir_mode *mode;
  int bad = 0;
  char buf[1024];

  switch (reason) {
  case dump_node_opcode_txt:
    name = get_irn_opname(n);
    fprintf(F, "%s", name);
    break;
  case dump_node_mode_txt:
    mode = get_irn_mode(n);
    name = get_irn_opname(n);

    if (mode && mode != mode_BB && mode != mode_ANY && mode != mode_BAD && mode != mode_T) {
      p = name + strlen(name) - 2;
      if (p[0] == '_' && p[1] == 'i') {
        tarval_snprintf(buf, sizeof(buf), get_Immop_tarval(n));
        fprintf(F, "[%s]", buf);
      }

      fprintf(F, "%s", get_mode_name(mode));
    }
    break;

  case dump_node_nodeattr_txt:
    break;
  }
  return bad;
}
