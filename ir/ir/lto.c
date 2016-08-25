#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>

#include "array.h"
#include "ircons_t.h"
#include "irflag_t.h"
#include "irgmod.h"
#include "irgraph_t.h"
#include "irgwalk.h"
#include "irio_t.h"
#include "irprintf.h"
#include "irprog_t.h"
#include "obst.h"
#include "panic.h"
#include "pmap.h"
#include "tv_t.h"
#include "util.h"

#include "lto.h"

/** The list of all currently known types. */
static ir_type** lto_known_types = NULL;

static int ir_file_count = -1;

/**
 * Flag whether an executable file is generated or not.
 */
static bool lto_is_compile_mode_executable = false;

static bool lto_shared_flag_set = false;


void lto_set_shared_flag(void) {
	lto_shared_flag_set = true;
}

bool lto_can_adjust_visibility(void){
	// TODO: determine when we are allowed to do this
	// For now it is permitted if not shared
	// and we are not exporting to an ir file again
	return !lto_shared_flag_set && lto_is_compile_mode_executable;
}

void lto_set_ir_file_count(int count);

void lto_set_ir_file_count(int count){
	ir_file_count = count;
}

/**
 * Sets a flag whether the target is to compile an executable file or not.
 */
void lto_set_mode_is_compile_executable(int is_executable){
	lto_is_compile_mode_executable = is_executable;
}

/**
 * Initializes the list of types. Can safely be called more than once.
 *
 */
void lto_init(){
	if(lto_known_types == NULL){
		lto_known_types = NEW_ARR_F(ir_type*, 0);

		// add the initial global types
		for(unsigned i = IR_SEGMENT_FIRST; i <= IR_SEGMENT_LAST; i++){
			ir_type *type = get_segment_type_(i);
			lto_register_type(type);
		}
	}
}

// TODO: when to call?
void lto_cleanup(){
	DEL_ARR_F(lto_known_types);
}

void lto_register_type(ir_type *type){
	ARR_APP1(ir_type*, lto_known_types, type);
}

/**
 * Searches the list of known types and retrieves the first
 * type with this name and matching opcode.
 */
ir_type *lto_find_type_by_name(ident* name, tp_opcode opcode)
{
	if(name == NULL)
		return NULL;

	for (size_t i = 0, n_modes = ARR_LEN(lto_known_types); i < n_modes; ++i) {
		ir_type *t = lto_known_types[i];

		if(t->name == NULL || t->opcode != opcode)
			continue;

		if(strcmp(name, t->name) == 0)
			return t;

	}
	return NULL;
}


static inline bool lto_primitive_equal(ir_type * comp, ir_mode *mode,
		unsigned size, unsigned align, unsigned flags ){
	// TODO: are there more things to compare?
	return comp->mode == mode && comp->size == size
			&& comp->align == align && comp->flags == flags;
}

ir_type* lto_find_primitive(ir_mode *mode, unsigned size, unsigned align,
		unsigned flags) {

	for (size_t i = 0, n_types= ARR_LEN(lto_known_types); i < n_types; ++i) {

		ir_type* type = lto_known_types[i];
		if (type->opcode == tpo_primitive) {

			if (lto_primitive_equal(type, mode, size, align, flags)) {
				return type;
			}
		}
	}

	return NULL;
}
/**
 * Retrieves the entity with the given name. Global entities must have
 * unique names.
 *
 */
ir_entity* lto_get_entity_by_name(const char* name) {
	ident *id = (ident *) name;
	pmap *globals = irp->globals;

	return pmap_get(ir_entity, globals, id);
}


ir_visibility lto_adaptVisibility(ir_visibility v1, ir_visibility v2){
	if(v1 == v2)
		return v1;

	// if there are different visibilities, choose the non external visibility
	if(v1 == ir_visibility_external){
		return v2;
	}else  if(v2 == ir_visibility_external){
		return v1;
	}else{
		panic("Incompatible visibilities: %+F vs. %+F\n", v1, v2);
		return v1;
	}
}


/**
 * If the function type differs from the already known function type
 * figure out which is the correct type. This method assumes that implicit
 * declared functions have zero parameters. Two function types with different
 * non zero parameter count are regarded as invalid.
 *
 */
ir_type* lto_handle_implicit_declared_functions(ir_type *old_type, ir_type *new_type){
	size_t new_N = new_type->attr.method.n_params;
	size_t old_N = old_type->attr.method.n_params;

	size_t new_RN = new_type->attr.method.n_res;
	size_t old_RN = old_type->attr.method.n_res;

	//TODO: what about variadic method types? Can they cause trouble
	bool new_variadic = new_type->attr.method.variadic;
	bool old_variadic = old_type->attr.method.variadic;

	// Since there is no information if a method type is from a definition,
	// a delceration or an impplicit declaration we try to infer this
	// information from the amout of parameters and return types
	// an implicit declaration has no parameters and an integer return value
	// Compare each parameter type is also not an option since ir_type s
	// are not merge and cannot be easily compared

	bool newCouldBeImplicit = new_N == 0 && new_RN == 1 && new_type->attr.method.res_type[0]->mode == get_modeIs();
	bool oldCouldBeImplicit = old_N == 0 && old_RN == 1 && old_type->attr.method.res_type[0]->mode == get_modeIs();


	// Implicit method type check
	if (new_RN != old_RN || new_N != old_N || new_variadic != old_variadic) {
		// Different amount of parameters or return values.
		// This is either an implicit declared function or an error
		// If it is an implicit declaration we go with the other type

		if (oldCouldBeImplicit) {
			return new_type;
		} else if (newCouldBeImplicit) {
			return old_type;
		}

		// TODO: since there could be a declaration X foo() and X foo(p1, p2, p3)
		// TODO: we trust the frontend to do its job
		if (new_N > old_N)
			return new_type;

		return old_type;
	}

	// TODO: figure out if one if them is the definition
	return old_type;
}


static unsigned unique_number = 0;
unsigned lto_unique_number(void){
	return unique_number++;
}

bool lto_is_visibility_private(ir_visibility visibility){
	return (visibility == ir_visibility_private) || (visibility == ir_visibility_local);
}
