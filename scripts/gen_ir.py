#!/usr/bin/env python
import sys
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
import ir_spec

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

env = Environment()
env.filters['argdecls']  = format_argdecls
env.filters['args']      = format_args
env.filters['blockdecl'] = format_blockdecl
env.filters['block']     = format_block
env.filters['curblock']  = format_curblock
env.filters['insdecl']       = format_insdecl
env.filters['arity_and_ins'] = format_arity_and_ins

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
		parent = ir_spec.nodes[node["is_a"]]
		node["ins"] = parent["ins"]
		if "outs" in parent:
			node["outs"] = parent["outs"]

	if "outs" in node:
		node["mode"] = "mode_T"
	if "nodbginfo" in node:
		node["db"] = "NULL"
		node["dbdecl"] = ""
		node["dbdeclnocomma"] = ""
	else:
		node["db"] = "db"
		node["dbdecl"] = "dbg_info *db, "
		node["dbdeclnocomma"] = "dbg_info *db"

	node.setdefault("ins", [])
	node.setdefault("arity", len(node["ins"]))
	node.setdefault("attrs", [])
	node.setdefault("constrname", nodename);
	node.setdefault("constructor_args", [])
	node.setdefault("attrs_name", nodename.lower())
	node.setdefault("block", "block")

	# construct node arguments
	arguments = [ ]
	initattrs = [ ]
	specialconstrs = [ ]
	for input in node["ins"]:
		arguments.append(dict(type = "ir_node *", name = "irn_" + input))

	# Special case for Builtin...
	if nodename == "Builtin":
		for attr in node["attrs"]:
			if attr["name"] == "kind":
				arguments.append(prepare_attr(attr))

	if node["arity"] == "variable":
		arguments.append(dict(type = "int", name = "arity"))
		arguments.append(dict(type = "ir_node **", name = "in"))

	if "mode" not in node:
		arguments.append(dict(type = "ir_mode *", name = "mode"))
		node["mode"] = "mode"

	attrs_with_special = 0
	for attr in node["attrs"]:
		if nodename == "Builtin" and attr["name"] == "kind":
			continue

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

node_template = env.from_string('''
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
	{% endfor %}
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

#############################

def main(argv):
	"""the main function"""

	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	gendir = argv[2]

	# List of TODOs
	niymap = ["Alloc", "Anchor", "ASM", "Bad", "Bound", "Break", "Builtin",
		"CallBegin", "Const", "Const_type", "Const_long", "CopyB",
		"defaultProj", "Dummy", "EndReg", "EndExcept",
		"Filter", "InstOf", "NoMem", "Phi", "Raise",
		"simpleSel", "SymConst", "SymConst_type", "Sync"]

	file = open(gendir + "/gen_ir_cons.c.inl", "w")
	for nodename, node in do_dictsort(ir_spec.nodes):
		if nodename in niymap:
			continue
		preprocess_node(nodename, node)
		if not "abstract" in node:
			file.write(node_template.render(vars()))

			if "special_constructors" in node:
				for special in node["special_constructors"]:
					node["constrname"] = special["constrname"]
					special["attr"]["init"] = special["attr"]["special"]["init"]
					file.write(node_template.render(vars()))
	file.write("\n")
	file.close()

if __name__ == "__main__":
	main(sys.argv)
