#!/usr/bin/env python
import sys
import re
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
from spec_util import is_dynamic_pinned, verify_node, isAbstract
import ir_spec

def error(msg):
	"""writes an error message to stderr"""
	sys.stderr.write("Error: " + msg + "\n");

def warning(msg):
	"""writes a warning message to stderr"""
	sys.stderr.write("Warning: " + msg + "\n");

def format_args(arglist):
	return "\n".join(arglist)

def format_ifnset(string, node, key):
	if hasattr(node, key):
		return ""
	return string

def format_block(node):
	if hasattr(node, "knownBlock"):
		if hasattr(node, "knownGraph"):
			return ""
		return "irg"
	else:
		return "preds[0]"

def format_arguments(string):
	args = re.split('\s*\n\s*', string)
	if args[0] == '':
		args = args[1:]
	if len(args) > 0 and args[-1] == '':
		args = args[:-1]
	return ", ".join(args)

env = Environment()
env.filters['args']      = format_args
env.filters['ifnset']    = format_ifnset
env.filters['block']     = format_block
env.filters['arguments'] = format_arguments

def get_io_type(type, attrname, node):
	if type == "ir_tarval*":
		importcmd = "ir_tarval *%s = read_tv(env);" % attrname
		exportcmd = "write_tarval(env, %(val)s);";
	elif type == "ir_mode*":
		importcmd = "ir_mode *%s = read_mode(env);" % attrname
		exportcmd = "write_mode(env, %(val)s);"
	elif type == "ir_entity*":
		importcmd = "ir_entity *%s = read_entity(env);" % attrname
		exportcmd = """write_entity_ref(env, %(val)s);"""
	elif type == "ir_type*":
		importcmd = "ir_type *%s = read_type(env);" % attrname
		exportcmd = """write_type_ref(env, %(val)s);"""
	elif type == "long" and node.name == "Proj":
		importcmd = "long %s = read_long(env);" % attrname
		exportcmd = """write_long(env, %(val)s);"""
	elif type == "pn_Cmp" or type == "ir_where_alloc":
		importcmd = "%s %s = (%s) read_long(env);" % (type, attrname, type)
		exportcmd = """write_long(env, (long) %(val)s);"""
	elif type == "ir_cons_flags" and node.name == "Store":
		importcmd = "ir_cons_flags %s = get_cons_flags(env);" % attrname
		exportcmd = """write_pin_state(env, irn);
			write_volatility(env, irn);
			write_align(env, irn);"""
	elif type == "ir_cons_flags" and node.name == "Load":
		importcmd = "ir_cons_flags %s = get_cons_flags(env);" % attrname
		exportcmd = """write_pin_state(env, irn);
			write_volatility(env, irn);
			write_align(env, irn);"""
	elif type == "op_pin_state":
		importcmd = "op_pin_state %s = read_pin_state(env);" % attrname
		exportcmd = "write_pin_state(env, irn);"
	elif type == "ir_builtin_kind":
		importcmd = "ir_builtin_kind %s = read_builtin_kind(env);" % attrname
		exportcmd = "write_builtin_kind(env, irn);"
	elif type == "cond_kind":
		importcmd = "cond_kind %s = read_cond_kind(env);" % attrname
		exportcmd = "write_cond_kind(env, irn);"
	elif type == "cond_jmp_predicate":
		importcmd = "cond_jmp_predicate %s = read_cond_jmp_predicate(env);" % attrname
		exportcmd = "write_cond_jmp_predicate(env, irn);"
	elif type == "int":
		importcmd = "int %s = read_int(env);" % attrname
		exportcmd = """write_int(env, %(val)s);"""
	elif type == "unsigned":
		importcmd = "unsigned %s = read_unsigned(env);" % attrname
		exportcmd = """write_unsigned(env, %(val)s);"""
	elif type == "long":
		importcmd = "long %s = read_long(env);" % attrname
		exportcmd = """write_long(env, %(val)s);"""
	else:
		warning("cannot generate import/export for node %s: unsupported attribute type: %s" % (node.name, type))
		importcmd = """// BAD: %s %s
			%s %s = (%s) 0;""" % (type, attrname, type, attrname, type)
		exportcmd = "// BAD: %s" % type
	return (importcmd, exportcmd)

def prepare_attr(node, attr):
	(importcmd,exportcmd) = get_io_type(attr["type"], attr["name"], node)
	attr["importcmd"] = importcmd
	attr["exportcmd"] = exportcmd % {"val": "get_%s_%s(irn)" % (node.name, attr["name"])}


def preprocess_node(node):
	# dynamic pin state means, we have to im/export that
	if is_dynamic_pinned(node):
		newattr = dict(
			name = "state",
			type = "op_pin_state"
		)
		if hasattr(node, "pinned_init"):
			newattr["init"] = node.pinned_init
		node.attrs.append(newattr)

	verify_node(node)

	# construct node arguments
	arguments = [ ]
	initargs = [ ]
	specialconstrs = [ ]
	i = 1
	for input in node.ins:
		arguments.append("preds[%i]" % i)
		i += 1

	if node.arity == "variable" or node.arity == "dynamic":
		arguments.append("numpreds - %i" % i)
		arguments.append("preds + %i" % i)

	if not hasattr(node, "mode"):
		arguments.append("mode")

	attrs_with_special = 0
	for attr in node.attrs:
		prepare_attr(node, attr)
		if "special" in attr:
			if not "init" in attr:
				warning("Node type %s has an attribute with a \"special\" entry but without \"init\"" % node.name)
				sys.exit(1)

			if attrs_with_special != 0:
				warning("Node type %s has more than one attribute with a \"special\" entry" % node.name)
				sys.exit(1)

			attrs_with_special += 1

			if "prefix" in attr["special"]:
				specialname = attr["special"]["prefix"] + node.name
			elif "suffix" in attr["special"]:
				specialname = node.name + attr["special"]["suffix"]
			else:
				error("Unknown special constructor type for node type %s" % node.name)
				sys.exit(1)

			specialconstrs.append(
				dict(
					constrname = specialname,
					attrname = attr["name"],
					value = attr["special"]["init"]
				)
			)
		elif "init" in attr:
			if attr["type"] == "op_pin_state":
				initfunc = "set_irn_pinned"
			else:
				initfunc = "set_" + node.name + "_" + attr["name"]
			initargs.append((attr["name"], initfunc))
		else:
			arguments.append(attr["name"])

	for arg in node.constructor_args:
		prepare_attr(node, arg)
		arguments.append(arg["name"])

	node.arguments = arguments
	node.initargs = initargs
	node.special_constructors = specialconstrs

export_attrs_template = env.from_string('''
	case iro_{{node.name}}:
		{{"write_mode(env, get_irn_mode(irn));"|ifnset(node,"mode")}}
		{% for attr in node.attrs %}{{attr.exportcmd}}
		{% endfor %}
		{% for attr in node.constructor_args %}{{attr.exportcmd}}
		{% endfor %}break;''')

import_attrs_template = env.from_string('''
	case iro_{{node.name}}:	{
		{{"ir_mode *mode = read_mode(env);"|ifnset(node,"mode")}}
		{% for attr in node.attrs %}
		{{attr.importcmd}}
		{% endfor -%}
		{% for attr in node.constructor_args %}
		{{attr.importcmd}}
		{% endfor -%}
		{% for special in node.special_constructors %}
		if ({{special.attrname}} == {{special.value}})
			newnode = new_r_{{special.constrname}}(
{%- filter arguments %}
{{node|block}}
{{node.arguments|args}}
{% endfilter %});
		else
		{% endfor -%}
		newnode = new_r_{{node.name}}(
{%- filter arguments %}
{{node|block}}
{{node.arguments|args}}
{% endfilter %});
		{% for (initarg, initfunc) in node.initargs %}
		{{initfunc}}(newnode, {{initarg}});
		{% endfor -%}
		break;
	}
''')

def main(argv):
	"""the main function"""

	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	gendir = argv[2]

	real_nodes = []
	for node in ir_spec.nodes:
		if isAbstract(node):
			continue
		real_nodes.append(node)

	file = open(gendir + "/gen_irio_export.inl", "w");
	file.write("/* Warning: automatically generated code */")
	for node in real_nodes:
		if node.customSerializer:
			continue

		preprocess_node(node)
		file.write(export_attrs_template.render(vars()))
	file.write("\n")
	file.close()

	file = open(gendir + "/gen_irio_import.inl", "w");
	file.write("/* Warning: automatically generated code */")
	for node in real_nodes:
		if node.customSerializer:
			continue
		file.write(import_attrs_template.render(vars()))
	file.write("\n")
	file.close()

	file = open(gendir + "/gen_irio_lex.inl", "w");
	file.write("/* Warning: automatically generated code */")
	for node in real_nodes:
		file.write("\tINSERT(tt_iro, \"%s\", iro_%s);\n" % (node.name, node.name));
	file.close()

main(sys.argv)
