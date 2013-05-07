#!/usr/bin/env python
#
# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import sys
from jinja2 import Environment, Template, FileSystemLoader
from spec_util import is_dynamic_pinned, isAbstract, load_spec
from filters import format_arguments, filter_isnot, filter_hasnot, filter_notset

def error(msg):
	"""writes an error message to stderr"""
	sys.stderr.write("Error: " + msg + "\n");

def warning(msg):
	"""writes a warning message to stderr"""
	sys.stderr.write("Warning: " + msg + "\n");

def format_args(arglist):
	return "\n".join(arglist)

def format_block(node):
	if hasattr(node, "knownBlock"):
		if hasattr(node, "knownGraph"):
			return ""
		return "env->irg"
	else:
		return "block"

env = Environment(loader=FileSystemLoader("."))
env.filters['args']      = format_args
env.filters['block']     = format_block
env.filters['arguments'] = format_arguments
env.filters['isnot']     = filter_isnot
env.filters['notset']    = filter_notset
env.filters['hasnot']    = filter_hasnot

def get_io_type(type, attrname, node):
	if type == "ir_tarval*":
		importcmd = "read_tarval(env)"
		exportcmd = "write_tarval(env, %(val)s);"
	elif type == "ir_mode*":
		importcmd = "read_mode_ref(env)"
		exportcmd = "write_mode_ref(env, %(val)s);"
	elif type == "ir_entity*":
		importcmd = "read_entity_ref(env)"
		exportcmd = "write_entity_ref(env, %(val)s);"
	elif type == "ir_type*":
		importcmd = "read_type_ref(env)"
		exportcmd = "write_type_ref(env, %(val)s);"
	elif type == "long":
		importcmd = "read_long(env)"
		exportcmd = "write_long(env, %(val)s);"
	elif type == "ir_relation":
		importcmd = "read_relation(env)"
		exportcmd = "write_relation(env, %(val)s);"
	elif type == "ir_align":
		importcmd = "read_align(env)"
		exportcmd = "write_align(env, %(val)s);"
	elif type == "ir_volatility":
		importcmd = "read_volatility(env)"
		exportcmd = "write_volatility(env, %(val)s);"
	elif type == "ir_cons_flags":
		importcmd = "cons_none"
		exportcmd = "" # can't really export cons_flags
	elif type == "op_pin_state":
		importcmd = "read_pin_state(env)"
		exportcmd = "write_pin_state(env, node);"
	elif type == "ir_builtin_kind":
		importcmd = "read_builtin_kind(env)"
		exportcmd = "write_builtin_kind(env, node);"
	elif type == "cond_kind":
		importcmd = "read_cond_kind(env)"
		exportcmd = "write_cond_kind(env, node);"
	elif type == "cond_jmp_predicate":
		importcmd = "read_cond_jmp_predicate(env)"
		exportcmd = "write_cond_jmp_predicate(env, node);"
	elif type == "int":
		importcmd = "read_int(env)"
		exportcmd = "write_int(env, %(val)s);"
	elif type == "unsigned":
		importcmd = "read_unsigned(env)"
		exportcmd = "write_unsigned(env, %(val)s);"
	elif type == "long":
		importcmd = "read_long(env)"
		exportcmd = "write_long(env, %(val)s);"
	elif type == "ir_switch_table*":
		importcmd = "read_switch_table(env)"
		exportcmd = "write_switch_table(env, %(val)s);"
	else:
		warning("cannot generate import/export for node %s: unsupported attribute type: %s" % (node.name, type))
		importcmd = "/* BAD: %s %s */ (%s)0" % (type, attrname, type)
		exportcmd = "// BAD: %s" % type
	return (importcmd, exportcmd)

def prepare_attr(node, attr):
	(importcmd,exportcmd) = get_io_type(attr["type"], attr["name"], node)
	attr["importcmd"] = importcmd
	attr["exportcmd"] = exportcmd % {"val": "get_%s_%s(node)" % (node.name, attr["name"])}


def preprocess_node(node):
	if node.customSerializer:
		return

	# construct node arguments
	arguments = [ ]
	extraattrs = [ ]
	for input in node.ins:
		arguments.append("in_%s" % input[0])

	if node.arity == "variable" or node.arity == "dynamic":
		arguments.append("n_preds")
		arguments.append("preds")

	if not hasattr(node, "mode"):
		arguments.append("mode")

	for attr in node.attrs:
		prepare_attr(node, attr)
		if "to_flags" in attr:
			node.constructorFlags = True
			attr['to_flags'] = attr['to_flags'] % (attr["name"])
		elif "init" in attr:
			extraattrs.append(attr)
		else:
			arguments.append(attr["name"])

	for arg in node.constructor_args:
		if arg['type'] != "ir_cons_flags" and arg['name'] != "flags":
			error("only ir_cons_flags constructor arg supported in irio")
			continue
		node.constructorFlags = True
		arguments.append("flags")

	node.arguments  = arguments
	node.extraattrs = extraattrs
	node.dynamic_pinned = is_dynamic_pinned(node)

def main(argv):
	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	specfile     = argv[1]
	templatefile = argv[2]

	spec  = load_spec(specfile)
	nodes = spec.nodes
	real_nodes = []
	for node in nodes:
		if isAbstract(node):
			continue
		preprocess_node(node)
		real_nodes.append(node)

	env.globals['nodes']   = real_nodes
	env.globals['spec']    = spec
	env.globals['warning'] = "/* Warning: automatically generated file */"
	env.globals['hasattr'] = hasattr

	template = env.get_template(templatefile)
	sys.stdout.write(template.render().encode("utf-8"))

if __name__ == "__main__":
	main(sys.argv)
