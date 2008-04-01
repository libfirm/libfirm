void *Arg_0(char *vec_or_scalar, char *memory_or_register, char *register_class);
void *Arg_1(char *vec_or_scalar, char *memory_or_register, char *register_class);
void *Arg_2(char *vec_or_scalar, char *memory_or_register, char *register_class);
void *Res(char *vec_or_scalar,   char *memory_or_register, char *register_class);
void Emit(char *emit_statement);
void Destroys(char *destroyd_register);
void Priority(int prio1, ...);
void CostSavings(int prio1, ...);

#define PRIORITY_CLEANUP 10000
