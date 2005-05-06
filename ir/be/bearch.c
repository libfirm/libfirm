/**
 * Processor architecture specification.
 * @author Sebastian Hack
 * @date 11.2.2005
 *
 * $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <string.h>

#include "bearch.h"

#include "pset.h"

#include "entity.h"
#include "ircons_t.h"

/* Needed for obstack copy */
#define bcopy(src,dst,n) memcpy(dst,src,n)

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

static int cmp_header(const void *a, const void *b)
{
	const arch_header_t *h1 = a;
	const arch_header_t *h2 = b;

	return !(h1->kind == h2->kind && h1->isa == h2->isa && strcmp(h1->name, h2->name) == 0);
}

/**
 * The obstack and pset where the arch data is stored.
 */
typedef struct _arch_data_t {
	struct obstack obst;			/**< Here is the data allocated. */
	pset *header_set;					/**< Here reside copies of the headers. */
} arch_data_t;

/**
 * Get the storage (obstack and pset) for the arch objects.
 * @return A struct containing both, the obst and pset where
 * the objects are allocated and their pointer are recorded.
 */
static INLINE arch_data_t *get_arch_data(void)
{
	static arch_data_t arch_data;
	static int inited = 0;

	if(!inited) {
		obstack_init(&arch_data.obst);
		arch_data.header_set = new_pset(cmp_header, 512);
		inited = 1;
	}

	return &arch_data;
}

/**
 * Dump all arch objects in the arch_data collection.
 */
static void dump_arch_data(void)
{
	void *p;
	arch_data_t *d = get_arch_data();
	static const char *kind_names[] = {
#define ARCH_OBJ(name,in_list)	#name,
#include "bearch_obj.def"
#undef ARCH_OBJ
		""
	};

	printf("arch set:\n");
	for(p = pset_first(d->header_set); p; p = pset_next(d->header_set)) {
		arch_header_t *header = p;
		printf("%20s %10s %10s\n", kind_names[header->kind], header->name,
				header->isa ? header->isa->header.name : "");
	}
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
 * @return A pointer to the object.
 */
static INLINE void *_arch_data_insert(arch_kind_t kind, arch_isa_t *isa,
		const char *name, size_t size)
{
	arch_data_t *ad = get_arch_data();
	const obj_info_t *info = &obj_info[kind];
	arch_header_t *header = obstack_alloc(&ad->obst, size);
	arch_header_t *res = NULL;

	memset(header, 0, size);
	header->name = name;
	header->kind = kind;
	header->isa = isa;
	header->is_new = 1;

	res = pset_insert(ad->header_set, header, hash_header(header));

	/*
	 * If the object is newly created and thus not yet present
	 * in the set, add it to the isa
	 * The inserted object was no isa, list it in the isa if this is
	 * desired.
	 */
	if(res->is_new && isa && info->listed_in_isa) {
		list_add(&res->list, &isa->heads[kind]);
	}

	/* if it was in the set, remove it from the obstack */
	if(!res->is_new)
		obstack_free(&ad->obst, header);

	/* Mark the object as NOT new. */
	res->is_new = 0;

	return res;
}

#define arch_data_insert(type_suffix, isa, name) \
	_arch_data_insert(arch_kind_ ## type_suffix, isa, name, sizeof(arch_ ## type_suffix ## _t))

static INLINE void *_arch_data_find(arch_kind_t kind, const arch_isa_t *isa, const char *name)
{
	arch_header_t header;

	header.kind = kind;
	header.isa = (arch_isa_t *) isa;
	header.name = name;

	return pset_find(get_arch_data()->header_set, &header, hash_header(&header));
}

#define arch_data_find(type_suffix, isa, name) \
	_arch_data_find(arch_kind_ ## type_suffix, isa, name)

arch_isa_t *arch_add_isa(const char *name)
{
	arch_isa_t *isa;
	int i;

	isa = arch_data_insert(isa, NULL, name);
	for(i = 0; i < arch_kind_last; ++i)
		INIT_LIST_HEAD(&isa->heads[i]);

	return isa;
}

arch_register_set_t *arch_add_register_set(arch_isa_t *isa,
		const arch_register_class_t *cls, const char *name)
{
	arch_register_set_t *set =
		_arch_data_insert(arch_kind_register_set, isa, name,
				sizeof(arch_register_set_t) + cls->n_regs * sizeof(set->regs[0]));

	set->reg_class = cls;
	memset(set->regs, 0, sizeof(set->regs[0]) * cls->n_regs);

	return set;
}

arch_register_class_t *arch_add_register_class(arch_isa_t *isa, const char *name, int n_regs)
{
	char buf[64];
	char *set_name;
	int i, n;

	arch_register_class_t *cls =
		_arch_data_insert(arch_kind_register_class, isa, name,
				sizeof(arch_register_class_t) + n_regs * sizeof(arch_register_t *));

	/* Make a name for the set contianing all regs in this class. */
	n = snprintf(buf, sizeof(buf), "%s$set", name);
	set_name = obstack_copy(&get_arch_data()->obst, buf, n);

	cls->n_regs = n_regs;

	/* make the set of all registers in this class */
	cls->set = arch_add_register_set(isa, cls, name);

	/* Add each register in this class to the set */
	for(i = 0; i < n_regs; ++i)
		cls->set->regs[i] = 1;

	return cls;
}

void arch_register_set_add_register(arch_register_set_t *set, int index)
{
	assert(index >= 0 && index < set->reg_class->n_regs);
	set->regs[index] = 1;
}

arch_register_t *arch_add_register(arch_register_class_t *cls, int index, const char *name)
{
	arch_register_t *reg = NULL;

	assert(index >= 0 && index < cls->n_regs);
	reg = _arch_data_insert(arch_kind_register, arch_obj_get_isa(cls), name,
			sizeof(arch_register_t));
	cls->regs[index] = reg;

	reg->index = index;
	reg->reg_class = cls;
	reg->flags = arch_register_flag_none;

	return reg;
}

arch_immediate_t *arch_add_immediate(arch_isa_t *isa, const char *name, ir_mode *mode)
{
	arch_immediate_t *imm = arch_data_insert(immediate, isa, name);
	imm->mode = mode;
	return imm;
}

/*
 * Size of each operand type which should be allocated in an irn.
 * Keep this list up to date with the arch_operand_type_t enum.
 */
static const size_t operand_sizes[] = {
#define ARCH_OPERAND_TYPE(name,size_in_irn) size_in_irn,
#include "bearch_operand_types.def"
#undef ARCH_OPERAND_TYPE
	0
};

/**
 * Determine the amount of bytes which has to be extra allocated when a
 * new ir node is made from a insn format.
 * This size depends on the operands specified in the insn format.
 * @param fmt The instruction format.
 * @return The number of bytes which the operands of an instruction
 * will need in an ir node.
 */
static INLINE int arch_get_operands_size(const arch_insn_format_t *fmt)
{
	int i, res = 0;

	for(i = 0; i < fmt->n_in + fmt->n_out; ++i) {
		arch_operand_type_t type = fmt->operands[i].type;

		assert(type > arch_operand_type_invalid && type < arch_operand_type_last);
		res += operand_sizes[type];
	}

	return res;
}

arch_insn_format_t *arch_add_insn_format(arch_isa_t *isa, const char *name, int n_in, int n_out)
{
	int i;

	arch_insn_format_t *fmt =
		_arch_data_insert(arch_kind_insn_format, isa, name,
				sizeof(arch_insn_format_t) + (n_in + n_out) * sizeof(arch_operand_t));

	fmt->n_in = n_in;
	fmt->n_out = n_out;

	/* initialize each operand with invalid. */
	for(i = 0; i < n_in + n_out; ++i)
		fmt->operands[i].type = arch_operand_type_invalid;

	return fmt;
}

arch_insn_t *arch_add_insn(arch_insn_format_t *fmt, const char *name)
{
	/* Get the size the operands will need in the irn. */
	int operands_size = arch_get_operands_size(fmt);

	/* Insert the insn into the isa. */
	arch_insn_t *insn = arch_data_insert(insn, arch_obj_get_isa(fmt), name);

	insn->format = fmt;
	insn->op = new_ir_op(get_next_ir_opcode(), name, op_pin_state_pinned, 0,
			oparity_dynamic, 0, sizeof(arch_irn_data_t) + operands_size);

	return insn;
}

arch_insn_format_t *arch_find_insn_format(const arch_isa_t *isa, const char *name)
{
	return arch_data_find(insn_format, isa, name);
}

arch_isa_t *arch_find_isa(const char *name)
{
	return arch_data_find(isa, NULL, name);
}

arch_insn_t *arch_find_insn(const arch_isa_t *isa, const char *name)
{
	return arch_data_find(insn, isa, name);
}

arch_immediate_t *arch_find_immediate(const arch_isa_t *isa, const char *name)
{
	return arch_data_find(immediate, isa, name);
}

arch_register_class_t *arch_find_register_class(const arch_isa_t *isa, const char *name)
{
	return arch_data_find(register_class, isa, name);
}

arch_register_set_t *arch_find_register_set(const arch_isa_t *isa, const char *name)
{
	return arch_data_find(register_set, isa, name);
}

arch_register_set_t *arch_get_register_set_for_class(arch_register_class_t *cls)
{
	return _arch_get_register_set_for_class(cls);
}

static INLINE arch_operand_t *_arch_set_operand(arch_insn_format_t *fmt, int pos,
		arch_operand_type_t type)
{
	arch_operand_t *operand;
	int ofs = arch_inout_to_index(fmt, pos);

	assert(ofs < fmt->n_in + fmt->n_out);

	operand = &fmt->operands[ofs];
	operand->type = type;
	return operand;
}

arch_operand_t *arch_set_operand_register_set(arch_insn_format_t *fmt,
		int pos, const arch_register_set_t *set)
{
	arch_operand_t *op = _arch_set_operand(fmt, pos, arch_operand_type_register_set);
	op->data.set = set;
	return op;
}

arch_operand_t *arch_set_operand_callback(arch_insn_format_t *fmt,
		int pos, arch_register_callback_t *cb)
{
	arch_operand_t *op = _arch_set_operand(fmt, pos, arch_operand_type_callback);
	op->data.callback = cb;
	return op;
}

arch_operand_t *arch_set_operand_immediate(arch_insn_format_t *fmt,
		int pos, const arch_immediate_t *imm)
{
	arch_operand_t *op = _arch_set_operand(fmt, pos, arch_operand_type_immediate);
	op->data.imm = imm;
	return op;
}

arch_operand_t *arch_set_operand_memory(arch_insn_format_t *fmt, int pos)
{
	arch_operand_t *op = _arch_set_operand(fmt, pos, arch_operand_type_memory);
	return op;
}

arch_operand_t *arch_set_operand_equals(arch_insn_format_t *fmt, int pos, int same_as_pos)
{
	arch_operand_t *op = _arch_set_operand(fmt, pos, arch_operand_type_equals);
	op->data.same_as_pos = same_as_pos;
	return op;
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

ir_mode *arch_get_unknown_mode(void)
{
	return mode_Is;
}
