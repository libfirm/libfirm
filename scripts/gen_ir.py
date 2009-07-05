#!/usr/bin/env python
import sys
import re
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
from spec_util import is_dynamic_pinned, verify_node, isAbstract, setdefault
from ir_spec import nodes

def format_argdecls(node, first = False, voidwhenempty = False):
	if len(node.arguments) == 0:
		if voidwhenempty:
			return "void"
		else:
			return ""

	arguments = map(lambda arg: arg["type"] + " " + arg["name"], node.arguments)
	res = ""
	if not first:
		res = ", "
	res += ", ".join(arguments)
	return res

def format_args(node, first = False):
	res = ""
	if not first and node.arguments != []:
		res = ", "

	arguments = map(lambda arg: arg["name"], node.arguments)
	res += ", ".join(arguments)
	return res

def format_blockdecl(node):
	if hasattr(node, "knownBlock"):
		return "ir_graph *irg"
	else:
		return "ir_node *block"

def format_irgassign(node):
	if hasattr(node, "knownBlock"):
		return ""
	else:
		return "ir_graph *irg = get_Block_irg(block);\n"

def format_block(node):
	if hasattr(node, "knownBlock"):
		return "irg"
	else:
		return "block"

def format_curblock(node):
	if hasattr(node, "knownBlock"):
		return "current_ir_graph"
	else:
		return "current_ir_graph->current_block"

def format_insdecl(node):
	arity = node.arity
	if arity == "variable" and len(node.ins) == 0 or arity == "dynamic" or arity == 0:
		return ""

	if arity == "variable":
		insarity = len(node.ins)
		res = "int r_arity = arity + " + `insarity` + ";\n\tir_node **r_in;\n\t" \
			+ "NEW_ARR_A(ir_node *, r_in, r_arity);\n\t"
		i = 0
		for input in node.ins:
			res += "r_in[" + `i` + "] = irn_" + input + ";\n\t"
			i += 1
		res += "memcpy(&r_in[" + `insarity` + "], in, sizeof(ir_node *) * arity);\n\t"
	else:
		res = "ir_node *in[" + `arity` + "];\n\t"
		i = 0
		for input in node.ins:
			res += "in[" + `i` + "] = irn_" + input + ";\n\t"
			i += 1
	return res

def format_arity_and_ins(node):
	arity = node.arity
	if arity == "dynamic":
		return "-1, NULL"
	elif arity == "variable":
		if len(node.ins) == 0:
			return "arity, in"
		else:
			return "r_arity, r_in"
	elif arity == 0:
		return "0, NULL"
	else:
		return `arity` + ", in"

def format_arity(node):
	if hasattr(node, "arity_override"):
		return node.arity_override
	arity = node.arity
	if arity == "dynamic":
		return "oparity_dynamic"
	if arity == "variable":
		return "oparity_variable"
	if arity == 0:
		return "oparity_zero"
	if arity == 1:
		return "oparity_unary"
	if arity == 2:
		return "oparity_binary"
	if arity == 3:
		return "oparity_trinary"
	return "oparity_any"

def format_pinned(node):
	pinned = node.pinned
	if pinned == "yes":
		return "op_pin_state_pinned"
	if pinned == "no":
		return "op_pin_state_floats"
	if pinned == "exception":
		return "op_pin_state_exc_pinned"
	if pinned == "memory":
		return "op_pin_state_mem_pinned"
	print "WARNING: Unknown pinned state %s in format pined" % pinned
	return ""

def format_flags(node):
	flags = map(lambda x : "irop_flag_" + x, node.flags)
	if flags == []:
		flags = [ "irop_flag_none" ]
	return " | ".join(flags)

def format_attr_size(node):
	if not hasattr(node, "attr_struct"):
		return "0"
	return "sizeof(%s)" % node.attr_struct

def format_opindex(node):
	if hasattr(node, "op_index"):
		return node.op_index
	return "-1"

def filter_isnot(list, flag):
	result = []
	for node in list:
		if hasattr(node, flag):
			continue
		result.append(node)
	return result

env = Environment()
env.filters['argdecls']      = format_argdecls
env.filters['args']          = format_args
env.filters['blockdecl']     = format_blockdecl
env.filters['irgassign']     = format_irgassign
env.filters['block']         = format_block
env.filters['curblock']      = format_curblock
env.filters['insdecl']       = format_insdecl
env.filters['arity_and_ins'] = format_arity_and_ins
env.filters['arity']         = format_arity
env.filters['pinned']        = format_pinned
env.filters['flags']         = format_flags
env.filters['attr_size']     = format_attr_size
env.filters['isnot']         = filter_isnot
env.filters['opindex']       = format_opindex

def prepare_attr(attr):
	if "init" in attr:
		return dict(type = attr["type"], name = attr["name"], init = attr["init"])
	else:
		return dict(type = attr["type"], name = attr["name"])

def preprocess_node(node):
	verify_node(node)

	setdefault(node, "attrs_name", node.name.lower())
	setdefault(node, "block", "block")

	# construct node arguments
	arguments = [ ]
	initattrs = [ ]
	specialconstrs = [ ]
	for input in node.ins:
		arguments.append(dict(type = "ir_node *", name = "irn_" + input))

	if node.arity == "variable" or node.arity == "dynamic":
		arguments.append(dict(type = "int", name = "arity"))
		arguments.append(dict(type = "ir_node **", name = "in"))

	if not hasattr(node, "mode"):
		arguments.append(dict(type = "ir_mode *", name = "mode"))
		node.mode = "mode"

	attrs_with_special = 0
	for attr in node.attrs:
		attr.setdefault("initname", "." + attr["name"])

		if "special" in attr:
			if not "init" in attr:
				print "Node type %s has an attribute with a \"special\" entry but without \"init\"" % node.name
				sys.exit(1)

			if attrs_with_special != 0:
				print "Node type %s has more than one attribute with a \"special\" entry" % node.name
				sys.exit(1)

			attrs_with_special += 1

			if "prefix" in attr["special"]:
				specialname = attr["special"]["prefix"] + node.name
			elif "suffix" in attr["special"]:
				specialname = node.name + attr["special"]["suffix"]
			else:
				print "Unknown special constructor type for node type %s" % node.name
				sys.exit(1)

			specialconstrs.append(
				dict(
					constrname = specialname,
					attr = attr
				)
			)
		elif not "init" in attr:
			arguments.append(prepare_attr(attr))

	# dynamic pin state means more constructor arguments
	if is_dynamic_pinned(node):
		if hasattr(node, "pinned_init"):
			initattrs.append(dict(
				initname = ".exc.pin_state",
				init     = node.pinned_init
			))
		else:
			node.constructor_args.append(
				dict(
					name = "pin_state",
					type = "op_pin_state"
				)
			)
			initattrs.append(dict(
				initname = ".exc.pin_state",
				init     = "pin_state"
			))

	for arg in node.constructor_args:
		arguments.append(prepare_attr(arg))
		if arg["type"] == "ir_cons_flags":
			name = arg["name"]
			initattrs.append(dict(initname = ".exc.pin_state",
				init = name + " & cons_floats ? op_pin_state_floats : op_pin_state_pinned"))
			initattrs.append(dict(initname = ".volatility",
				init = name + " & cons_volatile ? volatility_is_volatile : volatility_non_volatile"))
			initattrs.append(dict(initname = ".aligned",
				init = name + " & cons_unaligned ? align_non_aligned : align_is_aligned"))

	node.arguments = arguments
	node.initattrs = initattrs
	node.special_constructors = specialconstrs

#############################

constructor_template = env.from_string('''

ir_node *new_rd_{{node.constrname}}(dbg_info *dbgi, {{node|blockdecl}}{{node|argdecls}})
{
	ir_node *res;
	ir_graph *rem = current_ir_graph;
	{{node|irgassign}}
	{{node|insdecl}}
	current_ir_graph = irg;
	res = new_ir_node(dbgi, irg, {{node.block}}, op_{{node.name}}, {{node.mode}}, {{node|arity_and_ins}});
	{% for attr in node.attrs -%}
		res->attr.{{node.attrs_name}}{{attr["initname"]}} =
		{%- if "init" in attr %} {{ attr["init"] -}};
		{%- else              %} {{ attr["name"] -}};
		{% endif %}
	{% endfor %}
	{%- for attr in node.initattrs -%}
		res->attr.{{node.attrs_name}}{{attr["initname"]}} = {{ attr["init"] -}};
	{%- endfor %}
	{{- node.init }}
	{% if node.optimize != False -%}
		res = optimize_node(res);
	{% endif -%}
	IRN_VRFY_IRG(res, irg);
	current_ir_graph = rem;
	return res;
}

ir_node *new_r_{{node.constrname}}({{node|blockdecl}}{{node|argdecls}})
{
	return new_rd_{{node.constrname}}(NULL, {{node|block}}{{node|args}});
}

ir_node *new_d_{{node.constrname}}(dbg_info *dbgi{{node|argdecls}})
{
	ir_node *res;
	{{ node.d_pre }}
	res = new_rd_{{node.constrname}}(dbgi, {{node|curblock}}{{node|args}});
	{{ node.d_post }}
	return res;
}

ir_node *new_{{node.constrname}}({{node|argdecls(True, True)}})
{
	return new_d_{{node.constrname}}(NULL{{node|args}});
}
''')

irnode_h_template = env.from_string('''
/* Warning: automatically generated code */

{% for node in nodes|isnot('custom_is') %}
static inline int _is_{{node.name}}(const ir_node *node)
{
	assert(node != NULL);
	return _get_irn_op(node) == op_{{node.name}};
}
{% endfor %}

{% for node in nodes %}
#define is_{{node.name}}(node)    _is_{{node.name}}(node)
{%- endfor %}

''')

irnode_template = env.from_string('''
/* Warning: automatically generated code */
{% for node in nodes %}
int (is_{{node.name}})(const ir_node *node)
{
	return _is_{{node.name}}(node);
}
{% endfor %}
''')

irop_template = env.from_string('''
/* Warning: automatically generated code */
{% for node in nodes %}
ir_op *op_{{node.name}}; ir_op *get_op_{{node.name}}(void) { return op_{{node.name}}; }
{%- endfor %}

void init_op(void)
{
	{% for node in nodes %}
	op_{{node.name}} = new_ir_op(iro_{{node.name}}, "{{node.name}}", {{node|pinned}}, {{node|flags}}, {{node|arity}}, {{node|opindex}}, {{node|attr_size}}, NULL);
	{%- endfor %}

	be_init_op();
}

void finish_op(void)
{
	{% for node in nodes %}
	free_ir_op(op_{{node.name}}); op_{{node.name}} = NULL;
	{%- endfor %}
}
''')

#############################

def main(argv):
	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	gendir = argv[2]

	# List of TODOs
	niymap = [ "ASM", "Const", "Phi", "SymConst", "Sync"]

	real_nodes = []
	for node in nodes:
		if isAbstract(node):
			continue
		real_nodes.append(node)

	file = open(gendir + "/gen_ir_cons.c.inl", "w")
	for node in real_nodes:
		preprocess_node(node)

		if node.name in niymap:
			continue

		if not isAbstract(node) and not hasattr(node, "singleton"):
			file.write(constructor_template.render(vars()))

			if hasattr(node, "special_constructors"):
				for special in node.special_constructors:
					node.constrname = special["constrname"]
					special["attr"]["init"] = special["attr"]["special"]["init"]
					file.write(constructor_template.render(vars()))
	file.write("\n")
	file.close()

	file = open(gendir + "/gen_irnode.h", "w")
	file.write(irnode_h_template.render(nodes = real_nodes))
	file.close()

	file = open(gendir + "/gen_irnode.c.inl", "w")
	file.write(irnode_template.render(nodes = real_nodes))
	file.close()

	file = open(gendir + "/gen_irop.c.inl", "w")
	file.write(irop_template.render(nodes = real_nodes))
	file.close()

main(sys.argv)
