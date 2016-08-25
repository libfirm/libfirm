#ifndef LIBFIRM_IR_IR_LTO_H_
#define LIBFIRM_IR_IR_LTO_H_


bool lto_can_adjust_visibility(void);

void lto_set_shared_flag(void);

void lto_set_mode_is_compile_executable(int is_executable);

void lto_init(void);

void lto_cleanup(void);

void lto_register_type(ir_type *type);

ir_type *lto_find_type_by_name(ident* name, tp_opcode opcode);

ir_type* lto_find_primitive(ir_mode *mode, unsigned size, unsigned align,
		unsigned flags);

ir_entity* lto_get_entity_by_name(const char* name);

ir_visibility lto_adaptVisibility(ir_visibility v1, ir_visibility v2);

ir_type* lto_handle_implicit_declared_functions(ir_type *entity, ir_type *type);


bool lto_is_visibility_private(ir_visibility visibility);

unsigned lto_unique_number(void);

#endif /* LIBFIRM_IR_IR_LTO_H_ */
