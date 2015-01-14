#!/usr/bin/env python
#
# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import sys
import argparse
from datetime import datetime
from jinja2 import Environment, Template, BaseLoader
from spec_util import is_dynamic_pinned, isAbstract, setdefault, load_spec, Attribute
from filters import arguments, filtjoin, has, hasnot

def parameterlist(parameterlist):
	return "\n".join(parameterlist)

def nodearguments(node):
	arguments = [arg.name for arg in node.arguments]
	return parameterlist(arguments)

def nodeparameters(node):
	parameters = ["%s %s" % (arg.type, arg.name) for arg in node.arguments]
	return parameterlist(parameters)

def nodeparametershelp(node):
	res = ""
	for param in node.arguments:
		res += " * @param %-9s %s\n" % (param.name, param.comment)
	return res

def a_an(text):
	if text[0] in "aAeEuUoOiI":
		return "an " + text
	return "a " + text

def blockparameter(node):
	if not node.block:
		return "ir_node *block"
	elif node.usesGraph:
		return "ir_graph *irg"
	else:
		return ""

def blockparameterhelp(node):
	if not node.block:
		return " * @param block     The IR block the node belongs to.\n"
	elif node.usesGraph:
		return " * @param irg       The IR graph the node belongs to.\n"
	else:
		return ""

def blockargument(node):
	if not node.block:
		return "block"
	elif node.usesGraph:
		return "irg"
	else:
		return ""

def blockassign(node):
	if node.block:
		return "ir_node *block = %s;" % node.block
	else:
		return ""

def irgassign(node):
	if node.usesGraph:
		return ""
	else:
		return "ir_graph *irg = get_irn_irg(block);\n"

def curblock(node):
	if not node.block:
		return "get_cur_block()"
	elif node.usesGraph:
		return "current_ir_graph"
	else:
		return ""

def insdecl(node):
	arity = node.arity
	if arity == "dynamic" or arity == "variable":
		if len(node.ins) == 0:
			return ""
		insarity = len(node.ins)
		res  = "int r_arity = arity + " + repr(insarity) + ";"
		res += "\n\tir_node **r_in= ALLOCAN(ir_node*, r_arity);"
		i = 0
		for input in node.ins:
			res += "\n\tr_in[" + repr(i) + "] = irn_" + input.name + ";"
			i += 1
		res += "\n\tmemcpy(&r_in[" + repr(insarity) + "], in, sizeof(ir_node *) * arity);\n\t"
	elif arity == 0:
		return ""
	else:
		res = "ir_node *in[" + repr(arity) + "];"
		i = 0
		for input in node.ins:
			res += "\n\tin[" + repr(i) + "] = irn_" + input.name + ";"
			i += 1
	return res

def arity_and_ins(node):
	arity = node.arity
	if arity == "dynamic" or arity == "variable":
		if len(node.ins) == 0:
			return "arity, in"
		else:
			return "r_arity, r_in"
	elif arity == 0:
		return "0, NULL"
	else:
		return repr(arity) + ", in"

def arity(node):
	if hasattr(node, "arity_override"):
		return node.arity_override
	arity = node.arity
	if arity == "dynamic":
		return "oparity_dynamic"
	if arity == "variable":
		return "oparity_variable"
	return "oparity_any"

def pinned(node):
	pinned = node.pinned
	if pinned == "yes":
		return "op_pin_state_pinned"
	if pinned == "no":
		return "op_pin_state_floats"
	if pinned == "exception":
		return "op_pin_state_exc_pinned"
	print("WARNING: Unknown pinned state %s in format pined" % pinned)
	return ""

def flags(node):
	flags = list(map(lambda x : "irop_flag_" + x, node.flags))
	if not flags:
		flags = [ "irop_flag_none" ]
	return " | ".join(flags)

def stringformat(string, *args):
	return string % args

def attr_size(node):
	if not hasattr(node, "attr_struct"):
		return "0"
	return "sizeof(%s)" % node.attr_struct

def opindex(node):
	if hasattr(node, "op_index"):
		return node.op_index
	return "-1"

keywords = frozenset([ "true", "false" ])
def escape_keywords(word):
	if word in keywords:
		return word + "_"
	return word

def parameters(string):
	return arguments(string, voidwhenempty = True)

def args(arglist):
	argument_names = [ arg.name for arg in arglist ]
	return "\n".join(argument_names)

def block(node):
	if not node.block:
		return "block"
	elif node.usesGraph:
		return "env->irg"
	else:
		return ""

def simplify_type(string):
	"""Returns a simplified version of a C type for use in a function name.
	Stars are replaced with _ref, spaces removed and the ir_ firm namespace
	prefix stripped."""
	res = string.replace("*", "_ref").replace(" ", "")
	if res.startswith("ir_"):
		res = res[3:]
	return res

def doxygrouplink(string, link=None):
	global tags
	if link == None:
		link = string
	if tags == None:
		return string
	e = tags.xpath("//compound[name/text()='%s']" % link)
	if len(e) == 0:
		return string
	e = e[0]
	anchorfile = e.xpath("filename/text()")
	if len(anchorfile) == 0:
		return string
	global linkbase
	return "<a href=\"%s%s\">%s</a>" % (linkbase, anchorfile[0], string)

tags = None
linkbase = None

def doxylink(string, link=None):
	global tags
	if link == None:
		link = string
	if tags == None:
		return string
	e = tags.xpath("//tagfile/compound[name/text()='%s']" % link)
	if len(e) == 0:
		return string
	e = e[0]
	anchorfile = e.xpath("anchorfile/text()")
	anchor = e.xpath("anchor/text()")
	if len(anchorfile) == 0 or len(anchor) == 0:
		return string
	global linkbase
	return "<a href=\"%s%s#%s\">%s</a>" % (linkbase, anchorfile[0], anchor[0], string)

def docutils(string):
	import docutils.writers.html4css1
	import docutils.core
	writer = docutils.writers.html4css1.Writer()
	document = docutils.core.publish_parts(string, writer=writer)['body']
	return document

def parse_tagfile(filename):
	global tags
	tagfile = open(filename)
	try:
		from lxml import etree
		tags = etree.parse(tagfile)
	except:
		tags = None

# simple FileSystemLoader variant. Compared to the default loader in jinja
# it does not perform searchpath magic and does not reject paths containig ".."
# for security.
# Note that we want to use a loader instead of simply env.from_string because
# we want to see the filename in error messages from jinja
class SimpleLoader(BaseLoader):
	def __init__(self):
		super(SimpleLoader, self).__init__()
		self.includedirs = [""]
	def get_source(self, environment, name):
		for dir in self.includedirs:
			try:
				path = name if dir == "" else "%s/%s" % (dir, name)
				contents = open(path).read()
			except:
				continue
			def uptodate():
				return False
			return contents, name, uptodate
		raise Exception("Could not open '%s'" % name)
	def list_template(self):
		return []

loader = SimpleLoader()
env = Environment(loader=loader, keep_trailing_newline=True)
env.filters['a_an']               = a_an
env.filters['args']               = args
env.filters['arguments']          = arguments
env.filters['arity_and_ins']      = arity_and_ins
env.filters['arity']              = arity
env.filters['attr_size']          = attr_size
env.filters['blockargument']      = blockargument
env.filters['block']              = block
env.filters['blockparameter']     = blockparameter
env.filters['blockparameterhelp'] = blockparameterhelp
env.filters['curblock']           = curblock
env.filters['escape_keywords']    = escape_keywords
env.filters['flags']              = flags
env.filters['filtjoin']           = filtjoin
env.filters['has']                = has
env.filters['hasnot']             = hasnot
env.filters['insdecl']            = insdecl
env.filters['blockassign']        = blockassign
env.filters['irgassign']          = irgassign
env.filters['nodearguments']      = nodearguments
env.filters['nodeparameters']     = nodeparameters
env.filters['nodeparametershelp'] = nodeparametershelp
env.filters['opindex']            = opindex
env.filters['parameterlist']      = parameterlist
env.filters['parameters']         = parameters
env.filters['pinned']             = pinned
env.filters['simplify_type']      = simplify_type
env.filters['stringformat']       = stringformat
env.filters['docutils']           = docutils
env.filters['doxylink']           = doxylink
env.filters['doxygrouplink']      = doxygrouplink

def preprocess_node(node):
	setdefault(node, "attrs_name", node.name.lower())

	# construct node arguments
	arguments = [ ]
	initattrs = [ ]
	for input in node.ins:
		arguments.append(
			Attribute("irn_" + input.name, type="ir_node *",
			          comment=input.name))

	if node.arity == "variable" or node.arity == "dynamic":
		arguments.append(
			Attribute("arity", type="int",
			          comment="size of additional inputs array"))
		arguments.append(
			Attribute("in", type="ir_node *const *",
			          comment="additional inputs"))

	if not hasattr(node, "mode"):
		arguments.append(
			Attribute("mode", type="ir_mode *",
			          comment = "mode of the operations result"))

	for attr in node.attrs:
		if attr.init is not None:
			continue
		arguments.append(attr)

	# dynamic pin state means more constructor arguments
	if is_dynamic_pinned(node):
		if hasattr(node, "pinned_init"):
			initattrs.append(
				Attribute("pin_state", fqname="exc.pin_state",
				          type="op_pin_state", init=node.pinned_init))
		else:
			node.constructor_args.append(
				Attribute("pin_state", type="op_pin_state",
				          comment = "pinned state"))
			initattrs.append(
				Attribute("pin_state", fqname="exc.pin_state",
				          type="op_pin_state", init="pin_state"))
	if hasattr(node, "throws_init"):
		initattrs.append(
			Attribute("throws_exception", fqname="exc.throws_exception",
			          type="unsigned", init=node.throws_init))

	for arg in node.constructor_args:
		arguments.append(arg)

	node.arguments = arguments
	node.initattrs = initattrs

def prepare_nodes(nodes):
	real_nodes = []
	abstract_nodes = []
	for node in nodes:
		if isAbstract(node):
			abstract_nodes.append(node)
		else:
			real_nodes.append(node)

	for node in real_nodes:
		preprocess_node(node)

	return (real_nodes, abstract_nodes)

def main(argv):
	parser = argparse.ArgumentParser(description='Generate code/docu from node specification', add_help=True)
	parser.add_argument('--tagfile', dest='tagfile', action='store',
	                    help='doxygen tag file for link generation')
	parser.add_argument('-I', dest='includedirs', action='store', nargs='*',
	                    default=[],
	                    help='include directories for template require directives')
	parser.add_argument('specfile', action='store',
	                    help='node specification file')
	parser.add_argument('templatefile', action='store',
	                    help='jinja2 template file')
	config = parser.parse_args()

	spec = load_spec(config.specfile)
	(nodes, abstract_nodes) = prepare_nodes(spec.nodes)

	env.globals['nodes']          = nodes
	env.globals['abstract_nodes'] = abstract_nodes
	env.globals['spec']    = spec
	env.globals['len']     = len
	env.globals['hasattr'] = hasattr
	env.globals['is_dynamic_pinned'] = is_dynamic_pinned
	env.globals['time'] = datetime.now().replace(microsecond=0).isoformat(' ')
	env.globals['warning'] = "/* Warning: automatically generated file */"
	loader.includedirs += config.includedirs
	template = env.get_template(config.templatefile)
	result = template.render()
	sys.stdout.write(result)

if __name__ == "__main__":
	main(sys.argv)
