#!/usr/bin/env python
import sys
import re
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
from spec_util import is_dynamic_pinned, verify_node
from ir_spec import nodes

def format_argdecls(node, first = False, voidwhenempty = False):
	if not node.has_key("args") or len(node["args"]) == 0:
		if voidwhenempty:
			return "void"
		else:
			return ""

	res = ""
	if not first:
		comma = ", "
	else:
		comma = ""
	for arg in node["args"]:
		res = res + (comma + arg["type"] + " " + arg["name"])
		comma = ", "
	return res

def format_args(node, first = False):
	if not node.has_key("args"):
		return ""

	res = ""
	if not first:
		comma = ", "
	else:
		comma = ""
	for arg in node["args"]:
		res = res + (comma + arg["name"])
		comma = ", "
	return res

def format_blockdecl(node):
	if node.get("knownBlock"):
		return ""
	else:
		return ", ir_node *block"

def format_block(node):
	if node.get("knownBlock"):
		return ""
	else:
		return ", block"

def format_curblock(node):
	if node.get("knownBlock"):
		return ""
	else:
		return ", current_ir_graph->current_block"

def format_insdecl(node):
	arity = node["arity"]
	if arity == "variable" and len(node["ins"]) == 0 or arity == "dynamic" or arity == 0:
		return ""

	if arity == "variable":
		insarity = len(node["ins"])
		res = "int r_arity = arity + " + `insarity` + ";\n\tir_node **r_in;\n\t" \
			+ "NEW_ARR_A(ir_node *, r_in, r_arity);\n\t"
		i = 0
		for input in node["ins"]:
			res += "r_in[" + `i` + "] = irn_" + input + ";\n\t"
			i += 1
		res += "memcpy(&r_in[" + `insarity` + "], in, sizeof(ir_node *) * arity);\n\t"
	else:
		res = "ir_node *in[" + `arity` + "];\n\t"
		i = 0
		for input in node["ins"]:
			res += "in[" + `i` + "] = irn_" + input + ";\n\t"
			i += 1
	return res

def format_arity_and_ins(node):
	arity = node["arity"]
	if arity == "dynamic":
		return "-1, NULL"
	elif arity == "variable":
		if len(node["ins"]) == 0:
			return "arity, in"
		else:
			return "r_arity, r_in"
	elif arity == 0:
		return "0, NULL"
	else:
		return `arity` + ", in"

def format_arity(node):
	if "arity_override" in node:
		return node["arity_override"]
	arity = node['arity']
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
	pinned = node["pinned"]
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
	flags = node['flags']
	flags = re.split("\s*,\s*", flags)
	flags = map(lambda x : "irop_flag_" + x, flags)
	return " | ".join(flags)

def format_attr_size(node):
	if "attr_struct" not in node:
		return "0"
	return "sizeof(%s)" % node['attr_struct']

def format_opindex(node):
	if "op_index" in node:
		return node["op_index"]
	return "-1"

def filter_isnot(list, flag):
	result = []
	for nodename, node in list:
		if flag in node:
			continue
		result.append((nodename, node))
	return result

env = Environment()
env.filters['argdecls']      = format_argdecls
env.filters['args']          = format_args
env.filters['blockdecl']     = format_blockdecl
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

def add_attr(list, type, name, init = None, initname = None):
	if initname == None:
		initname = "." + name
	if init != None:
		list.append(dict(type = type, name = name, init = init, initname = initname))
	else:
		list.append(dict(type = type, name = name, initname = initname))

def prepare_attr(attr):
	if "init" in attr:
		return dict(type = attr["type"], name = attr["name"], init = attr["init"])
	else:
		return dict(type = attr["type"], name = attr["name"])

def preprocess_node(nodename, node):
	# set default attributes
	if "is_a" in node:
		parent = nodes[node["is_a"]]
		node["ins"] = parent["ins"]
		if "op_index" in parent:
			node["op_index"] = parent["op_index"]
		if "pinned" in parent:
			node["pinned"] = parent["pinned"]
		if "outs" in parent:
			node["outs"] = parent["outs"]

	if "outs" in node:
		node["mode"] = "mode_T"
	node["db"] = "db"
	node["dbdecl"] = "dbg_info *db, "
	node["dbdeclnocomma"] = "dbg_info *db"

	if "flags" not in node and "abstract" not in node:
		print "WARNING: no flags specified for %s (you should say at least 'none')\n" % nodename

	node.setdefault("ins", [])
	node.setdefault("arity", len(node["ins"]))
	node.setdefault("attrs", [])
	node.setdefault("constrname", nodename);
	node.setdefault("constructor_args", [])
	node.setdefault("attrs_name", nodename.lower())
	node.setdefault("block", "block")
	node.setdefault("flags", "none")

	verify_node(nodename, node)

	# construct node arguments
	arguments = [ ]
	initattrs = [ ]
	specialconstrs = [ ]
	for input in node["ins"]:
		arguments.append(dict(type = "ir_node *", name = "irn_" + input))

	if node["arity"] == "variable" or node["arity"] == "dynamic":
		arguments.append(dict(type = "int", name = "arity"))
		arguments.append(dict(type = "ir_node **", name = "in"))

	if "mode" not in node:
		arguments.append(dict(type = "ir_mode *", name = "mode"))
		node["mode"] = "mode"

	attrs_with_special = 0
	for attr in node["attrs"]:
		attr.setdefault("initname", "." + attr["name"])

		if "special" in attr:
			if not "init" in attr:
				print "Node type %s has an attribute with a \"special\" entry but without \"init\"" % nodename
				sys.exit(1)

			if attrs_with_special != 0:
				print "Node type %s has more than one attribute with a \"special\" entry" % nodename
				sys.exit(1)

			attrs_with_special += 1

			if "prefix" in attr["special"]:
				specialname = attr["special"]["prefix"] + nodename
			elif "suffix" in attr["special"]:
				specialname = nodename + attr["special"]["suffix"]
			else:
				print "Unknown special constructor type for node type %s" % nodename
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
		if "pinned_init" in node:
			initattrs.append(dict(
				initname = ".exc.pin_state",
				init     = node["pinned_init"]
			))
		else:
			node["constructor_args"].append(
				dict(
					name = "pin_state",
					type = "op_pin_state"
				)
			)
			initattrs.append(dict(
				initname = ".exc.pin_state",
				init     = "pin_state"
			))

	for arg in node["constructor_args"]:
		arguments.append(prepare_attr(arg))
		if arg["type"] == "ir_cons_flags":
			name = arg["name"]
			initattrs.append(dict(initname = ".exc.pin_state",
				init = name + " & cons_floats ? op_pin_state_floats : op_pin_state_pinned"))
			initattrs.append(dict(initname = ".volatility",
				init = name + " & cons_volatile ? volatility_is_volatile : volatility_non_volatile"))
			initattrs.append(dict(initname = ".aligned",
				init = name + " & cons_unaligned ? align_non_aligned : align_is_aligned"))

	node["args"] = arguments
	node["initattrs"] = initattrs
	node["special_constructors"] = specialconstrs

#############################

constructor_template = env.from_string('''

ir_node *new_rd_{{node["constrname"]}}({{node["dbdecl"]}}ir_graph *irg{{node|blockdecl}}{{node|argdecls}})
{
	ir_node *res;
	ir_graph *rem = current_ir_graph;
	{{node|insdecl}}
	current_ir_graph = irg;
	res = new_ir_node({{node["db"]}}, irg, {{node["block"]}}, op_{{nodename}}, {{node["mode"]}}, {{node|arity_and_ins}});
	{% for attr in node["attrs"] -%}
		res->attr.{{node["attrs_name"]}}{{attr["initname"]}} =
		{%- if "init" in attr %} {{ attr["init"] -}};
		{%- else              %} {{ attr["name"] -}};
		{% endif %}
	{% endfor %}
	{%- for attr in node["initattrs"] -%}
		res->attr.{{node["attrs_name"]}}{{attr["initname"]}} = {{ attr["init"] -}};
	{%- endfor %}
	{{- node["init"] }}
	{% if node["optimize"] != False -%}
		res = optimize_node(res);
	{% endif -%}
	IRN_VRFY_IRG(res, irg);
	current_ir_graph = rem;
	return res;
}

ir_node *new_r_{{node["constrname"]}}(ir_graph *irg{{node|blockdecl}}{{node|argdecls}})
{
	{% if node["nodbginfo"] -%}
		return new_rd_{{node["constrname"]}}(irg{{node|block}}{{node|args}});
	{%- else -%}
		return new_rd_{{node["constrname"]}}(NULL, irg{{node|block}}{{node|args}});
	{%- endif %}
}

ir_node *new_d_{{node["constrname"]}}({{node["dbdeclnocomma"]}}{{node|argdecls(node["nodbginfo"])}})
{
	ir_node *res;
	{{ node["d_pre"] }}
	{% if node["nodbginfo"] -%}
		res = new_rd_{{node["constrname"]}}(current_ir_graph{{node|curblock}}{{node|args}});
	{%- else -%}
		res = new_rd_{{node["constrname"]}}(db, current_ir_graph{{node|curblock}}{{node|args}});
	{%- endif %}
	{{ node["d_post"] }}
	return res;
}

ir_node *new_{{node["constrname"]}}({{node|argdecls(True, True)}})
{
	{% if node["nodbginfo"] -%}
		return new_d_{{node["constrname"]}}({{node|args(True)}});
	{%- else -%}
		return new_d_{{node["constrname"]}}(NULL{{node|args}});
	{%- endif %}
}
''')

irnode_h_template = env.from_string('''
/* Warning: automatically generated code */

{% for nodename, node in nodes|isnot('custom_is') %}
static inline int _is_{{nodename}}(const ir_node *node)
{
	assert(node != NULL);
	return _get_irn_op(node) == op_{{nodename}};
}
{% endfor %}

{% for nodename, node in nodes %}
#define is_{{nodename}}(node)    _is_{{nodename}}(node)
{%- endfor %}

''')

irnode_template = env.from_string('''
/* Warning: automatically generated code */
{% for nodename, node in nodes %}
int (is_{{nodename}})(const ir_node *node)
{
	return _is_{{nodename}}(node);
}
{% endfor %}
''')

irop_template = env.from_string('''
/* Warning: automatically generated code */
{% for nodename, node in nodes %}
ir_op *op_{{nodename}}; ir_op *get_op_{{nodename}}(void) { return op_{{nodename}}; }
{%- endfor %}

void init_op(void)
{
	{% for nodename, node in nodes %}
	op_{{nodename}} = new_ir_op(iro_{{nodename}}, "{{nodename}}", {{node|pinned}}, {{node|flags}}, {{node|arity}}, {{node|opindex}}, {{node|attr_size}}, NULL);
	{%- endfor %}

	be_init_op();
}

void finish_op(void)
{
	{% for nodename, node in nodes %}
	free_ir_op(op_{{nodename}}); op_{{nodename}} = NULL;
	{%- endfor %}
}
''')

#############################

def main(argv):
	"""the main function"""

	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	gendir = argv[2]

	# List of TODOs
	niymap = [ "ASM", "Const", "Phi", "SymConst", "Sync"]

	file = open(gendir + "/gen_ir_cons.c.inl", "w")
	for nodename, node in do_dictsort(nodes):
		preprocess_node(nodename, node)
		if nodename in niymap:
			continue
		if "abstract" not in node and "singleton" not in node:
			file.write(constructor_template.render(vars()))

			if "special_constructors" in node:
				for special in node["special_constructors"]:
					node["constrname"] = special["constrname"]
					special["attr"]["init"] = special["attr"]["special"]["init"]
					file.write(constructor_template.render(vars()))
	file.write("\n")
	file.close()

	real_nodes = dict()
	for nodename, node in nodes.iteritems():
		if "abstract" in node:
			continue
		real_nodes[nodename] = node
	real_nodes = do_dictsort(real_nodes)

	file = open(gendir + "/gen_irnode.h", "w")
	file.write(irnode_h_template.render(nodes = real_nodes))
	file.close()

	file = open(gendir + "/gen_irnode.c.inl", "w")
	file.write(irnode_template.render(nodes = real_nodes))
	file.close()

	file = open(gendir + "/gen_irop.c.inl", "w")
	file.write(irop_template.render(nodes = real_nodes))
	file.close()

if __name__ == "__main__":
	main(sys.argv)
