/**
 * Processor architecture specification.
 * @author Sebastian Hack
 * @date 11.2.2005
 *
 * $Id$
 */

#include "bearch_t.h"

#include "firm_config.h"
#include "set.h"

#include "entity.h"
#include "ircons_t.h"

#if 1 /* HAVE_ALLOCA_H */
#include <alloca.h>
#endif /* HAVE_ALLOCA_H */

#define INIT_HEADER(tgt, kind_suffix, a_isa, str) \
	do { \
		arch_header_t *h = (arch_header_t *) (tgt); \
		memset(tgt, 0, sizeof(*(tgt))); \
		h->kind = arch_kind_ ## kind_suffix; \
		h->name = new_id_from_str(str); \
		h->isa = a_isa; \
	} while(0)

static INLINE int hash_header(const arch_header_t *header)
{
	int res = HASH_PTR(header->isa);
	res = 37 * res + HASH_STR(header->name, strlen(header->name));
	res = 37 * res + header->kind;
	return res;
}

static int cmp_header(const void *a, const void *b, size_t size)
{
	const arch_header_t *h1 = a;
	const arch_header_t *h2 = b;

	return !(h1->kind == h2->kind && strcmp(h1->name, h2->name) == 0);
}

static set *arch_data = NULL;

static set *get_arch_data(void)
{
	if(!arch_data)
		arch_data = new_set(cmp_header, 256);

	return arch_data;
}

typedef struct _obj_info_t {
	const char *name;
	int listed_in_isa;
	size_t size;
} obj_info_t;

static const obj_info_t obj_info[] = {
#define ARCH_OBJ(name,listed_in_isa)		{ #name, listed_in_isa, sizeof(arch_ ## name ## _t) },
#include "bearch_obj.def"
#undef ARCH_OBJ
	{ 0 }
};

/**
 * Insert an arch object to the global arch obj storage.
 *
 * If the object has already been created there, nothing is done and
 * the old object is created.
 *
 * @param kind The kind of the arch object.
 * @param isa The isa the object belongs to or NULL if it is the isa
 * itself.
 * @param name The name of the object.
 * @param was_new A pointer to an int where 1/0 is stored if the
 * object was created or already present. If NULL, it is simply ignored.
 * @return A pointer to the object.
 */
static INLINE void *_arch_data_insert(arch_kind_t kind, arch_isa_t *isa,
		const char *name, size_t size, int *was_new)
{
	const obj_info_t *info = &obj_info[kind];
	arch_header_t *data = alloca(size);
	arch_header_t *res = NULL;

	memset(data, 0, size);
	data->kind = kind;
	data->isa = isa;
	data->name = get_id_str(new_id_from_str(name));
	data->is_new = 1;

	res = set_insert(get_arch_data(), data, size, hash_header(data));

	/* If the object is newly created and thus not yet present
	 * in the set, add it to the isa */
	if(res->is_new) {

		/*
		 * The inserted object was no isa, list it in the isa if this is
		 * desired.
		 */
		if(isa && info->listed_in_isa)
			list_add(&res->list, &isa->heads[kind]);

		/* The inserted object is an isa, so initialize all its list heads. */
		else {
			int i;
			arch_isa_t *isa = (arch_isa_t *) res;

			for(i = 0; i < arch_kind_last; ++i)
				INIT_LIST_HEAD(&isa->heads[i]);
		}
	}

	/*
	 * If the caller wants to know, of the object was newly created,
	 * give it to him.
	 */
	if(was_new)
		*was_new = res->is_new;

	/* Mark the object as NOT new. */
	res->is_new = 0;

	return res;
}

#define arch_data_insert(type_suffix, isa, name, was_new) \
	_arch_data_insert(arch_kind_ ## type_suffix, isa, name, sizeof(arch_ ## type_suffix ## _t), was_new)

static INLINE void *_arch_data_find(arch_kind_t kind, const arch_isa_t *isa, const char *name)
{
	arch_header_t header;

	memset(&header, 0, sizeof(header));
	header.kind = kind;
	header.isa = (arch_isa_t *) isa;
	header.name = name;

	return set_find(get_arch_data(), &header, sizeof(header), hash_header(&header));
}

#define arch_data_find(type_suffix, isa, name) \
	_arch_data_find(arch_kind_ ## type_suffix, isa, name)

arch_isa_t *arch_add_isa(const char *name)
{
	return arch_data_insert(isa, NULL, name, NULL);
}

arch_register_class_t *arch_add_register_class(arch_isa_t *isa, const char *name, int n_regs)
{
	arch_register_class_t *cls =
		_arch_data_insert(arch_kind_register_class, isa, name,
				sizeof(arch_register_class_t) + n_regs * sizeof(arch_register_t *), NULL);

	cls->n_regs = n_regs;

	return cls;
}

arch_register_t *arch_add_register(arch_register_class_t *cls, int index, const char *name)
{
	arch_register_t *reg = NULL;

	assert(index >= 0 && index < cls->n_regs);
	reg = _arch_data_insert(arch_kind_register, arch_obj_get_isa(cls), name,
			sizeof(arch_register_t), NULL);
	cls->regs[index] = reg;

	reg->index = index;
	reg->reg_class = cls;
	reg->flags = arch_register_flag_none;

	return reg;
}

arch_immediate_t *arch_add_immediate(arch_isa_t *isa, const char *name, ir_mode *mode)
{
	arch_immediate_t *imm = arch_data_insert(immediate, isa, name, NULL);
	imm->mode = mode;
	return imm;
}

static const size_t operand_sizes[] = {
	0,
	0,
	sizeof(entity *),
	sizeof(arch_register_t *),
	sizeof(tarval *)
};

arch_insn_format_t *arch_add_insn_format(arch_isa_t *isa, const char *name, int n_in, int n_out)
{
	int i;

	arch_insn_format_t *fmt =
		_arch_data_insert(arch_kind_insn_format, isa, name,
				sizeof(arch_insn_format_t) + (n_in + n_out) * sizeof(arch_operand_type_t), NULL);

	fmt->n_in = n_in;
	fmt->n_out = n_out;
	fmt->irn_data_size = 0;

	/*
	 * Compute the number of bytes which must be extra allocated if this
	 * opcode is instantiated.
	 */
	for(i = 0; i < fmt->n_in; ++i) {
		arch_operand_t *op = arch_get_in_operand(fmt, i);
		op->offset_in_irn_data = fmt->irn_data_size;
		fmt->irn_data_size += operand_sizes[op->type];
	}

	if(fmt->n_out == 1) {
		arch_operand_t *op = arch_get_in_operand(fmt, i);
		op->offset_in_irn_data = fmt->irn_data_size;
		fmt->irn_data_size += operand_sizes[op->type];
	}

	return fmt;
}

arch_insn_t *arch_add_insn(arch_insn_format_t *fmt, const char *name)
{
	/* Insert the insn into the isa. */
	arch_insn_t *insn = arch_data_insert(insn, arch_obj_get_isa(fmt), name, NULL);

	insn->format = fmt;
	insn->op = new_ir_op(get_next_ir_opcode(), name, op_pin_state_pinned, 0,
			oparity_dynamic, 0, sizeof(arch_irn_data_t) + fmt->irn_data_size);

	return insn;
}

arch_insn_format_t *arch_find_insn_format(arch_isa_t *isa, const char *name)
{
	return arch_data_find(insn_format, isa, name);
}

arch_isa_t *arch_find_isa(const char *name)
{
	return arch_data_find(isa, NULL, name);
}

arch_register_class_t *arch_find_register_class_t(arch_isa_t *isa, const char *name)
{
	return arch_data_find(register_class, isa, name);
}

arch_register_set_t *arch_get_register_set_for_class(arch_register_class_t *cls)
{
	return _arch_get_register_set_for_class(cls);
}

ir_node *arch_new_node(const arch_insn_t *insn, ir_graph *irg, ir_node *block,
		ir_mode *mode, int arity, ir_node **in)
{
	ir_node *irn = new_ir_node(NULL, irg, block, insn->op, mode, arity, in);
	arch_irn_data_t *data = (void *) &irn->attr;

	data->magic = ARCH_IRN_FOURCC;
	data->insn = insn;

	return irn;
}

ir_node *arch_new_node_bare(const arch_insn_t *insn, ir_graph *irg, int arity)
{
	int i;
	ir_node **in = alloca(sizeof(in[0]) * arity);

	for(i = 0; i < arity; ++i)
		in[i] = new_Unknown(mode_Is);

	return arch_new_node(insn, irg, new_Unknown(mode_BB), mode_Is, arity, in);
}
