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

def format_simplify_type(string):
	"""Returns a simplified version of a C type for use in a function name.
	Stars are replaced with _ref, spaces removed and the ir_ firm namespace
	prefix stripped."""
	res = string.replace("*", "_ref").replace(" ", "")
	if res.startswith("ir_"):
		res = res[3:]
	return res

env = Environment(loader=FileSystemLoader([".", "/"]), keep_trailing_newline=True)
env.filters['args']          = format_args
env.filters['block']         = format_block
env.filters['arguments']     = format_arguments
env.filters['simplify_type'] = format_simplify_type
env.filters['isnot']         = filter_isnot
env.filters['notset']        = filter_notset
env.filters['hasnot']        = filter_hasnot

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
		print("usage: %s specname(ignored) destdirectory" % argv[0])
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
	result = template.render()
	sys.stdout.write(result)

if __name__ == "__main__":
	main(sys.argv)
