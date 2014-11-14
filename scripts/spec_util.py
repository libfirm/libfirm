# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import sys
import imp

abstracts = set()
def abstract(cls):
	abstracts.add(cls)
	return cls
def isAbstract(nodetype):
	return nodetype in abstracts

def op(cls):
	cls.__is_firm_op = True
	return cls
def isOp(nodetype):
	return hasattr(nodetype, "__is_firm_op")

class Attribute(object):
	def __init__(self, name, type, comment="", init=None, to_flags=None, noprop=False, fqname=None):
		self.type     = type
		self.name     = name
		self.comment  = comment
		self.init     = init
		self.to_flags = to_flags
		self.noprop   = noprop
		if fqname is None:
			fqname = name
		self.fqname = fqname

def is_dynamic_pinned(node):
	return node.pinned == "exception"

def is_fragile(node):
	return hasattr(node, "flags") and "fragile" in node.flags

def inout_contains(l, name):
	for entry in l:
		if entry.name == name:
			return True
	return False

def verify_node(node):
	if node.pinned not in ["yes", "no", "exception"]:
		print("%s: UNKNOWN PINNED MODE: %s" % (node.name, node.pinned))

	if not hasattr(node, "flags"):
		if not isAbstract(node):
			print("WARNING: no flags specified for %s\n" % node.name)
	elif type(node.flags) != list:
		print("ERROR: flags of %s not a list" % node.name)

	if hasattr(node, "pinned_init") and not is_dynamic_pinned(node):
		print("ERROR: node %s has pinned_init attribute but is not marked as dynamically pinned" % node.name)
	if hasattr(node, "flags") and "uses_memory" in node.flags:
		if not inout_contains(node.ins, "mem"):
			print("ERROR: memory op %s needs an input named 'mem'" % node.name)
	if is_fragile(node):
		if not is_dynamic_pinned(node):
			print("ERROR: fragile node %s must be dynamically pinned" % node.name)
		if not hasattr(node, "throws_init"):
			print("ERROR: fragile node %s needs a throws_init attribute" % node.name)
		if not inout_contains(node.outs, "X_regular"):
			print("ERROR: fragile node %s needs an output named 'X_regular'" % node.name)
		if not inout_contains(node.outs, "X_except"):
			print("ERROR: fragile node %s needs an output named 'X_except'" % node.name)
	else:
		if hasattr(node, "throws_init"):
			print("ERROR: throws_init only makes sense for fragile nodes")


def setldefault(node, attr, val):
	# Don't use hasattr, as these things should not be inherited
	if attr not in node.__dict__:
		setattr(node, attr, val)

def setdefault(node, attr, val):
	# Don't use hasattr, as these things should not be inherited
	if not hasattr(node, attr):
		setattr(node, attr, val)

class Operand(object):
	pass

def Input(name, comment=None):
	op = Operand()
	op.name = name
	op.comment = comment
	return op

def Output(name, comment=None):
	op = Operand()
	op.name = name
	op.comment = comment
	return op

def setnodedefaults(node):
	setldefault(node, "name", node.__name__)
	setdefault(node, "pinned", "no")
	if isAbstract(node):
		return

	setdefault(node, "ins", [])
	setdefault(node, "arity", len(node.ins))
	setdefault(node, "attrs", [])
	setdefault(node, "constructor_args", [])
	setdefault(node, "customSerializer", False)
	setdefault(node, "block", None)
	if hasattr(node, "__doc__"):
		node.doc = trim_docstring(node.__doc__)
	else:
		node.doc = ""
	if hasattr(node, "outs") and len(node.outs) > 1:
		node.mode = "mode_T"
	if "start_block" in node.flags:
		node.block = "get_irg_start_block(irg)"
	setdefault(node, "usesGraph", node.block != None)

	# As a shortcut you can specify inputs either as a list of strings or
	# as a list of (name, comment) tuples. Normalize it to Input objects
	new_ins = []
	for i in node.ins:
		if isinstance(i, basestring):
			i = Input(i)
		elif isinstance(i, tuple):
			i = Input(name=i[0], comment=i[1])
		new_ins.append(i)
	node.ins = new_ins
	if hasattr(node, "outs"):
		new_outs = []
		for o in node.outs:
			if isinstance(o, basestring):
				o = Output(o)
			elif isinstance(o, tuple):
				o = Output(name=o[0], comment=o[1])
			new_outs.append(o)
		node.outs = new_outs

def collect_ops(moduledict):
	return [node for node in moduledict.values() if isOp(node) ]

def setdefaults(nodes):
	for node in nodes:
		setnodedefaults(node)
	return nodes

def verify_spec(spec):
	if len(spec.nodes) == 0:
		sys.stderr.write("Warning: No nodes found in spec\n")
	if not hasattr(spec, "name"):
		sys.stderr.write("Warning: No name specified in node spec\n")

def load_spec(filename):
	module = imp.load_source('spec', filename)
	nodes = []
	for x in module.__dict__.values():
		if not isOp(x):
			continue
		setnodedefaults(x)
		verify_node(x)
		nodes.append(x)
	nodes.sort(key=lambda x: x.name)
	module.nodes = nodes
	if len(nodes) == 0:
		print("Warning: No nodes found in spec file '%s'" % filename)
	if not hasattr(module, "name"):
		print("Warning: No name specified in file '%s'" % filename)
	return module

def trim_docstring(docstring):
    if not docstring:
        return ''
    # Convert tabs to spaces (following the normal Python rules)
    # and split into a list of lines:
    lines = docstring.expandtabs().splitlines()
    # Determine minimum indentation (first line doesn't count):
    indent = sys.maxsize
    for line in lines[1:]:
        stripped = line.lstrip()
        if stripped:
            indent = min(indent, len(line) - len(stripped))
    # Remove indentation (first line is special):
    trimmed = [lines[0].strip()]
    if indent < sys.maxsize:
        for line in lines[1:]:
            trimmed.append(line[indent:].rstrip())
    # Strip off trailing and leading blank lines:
    while trimmed and not trimmed[-1]:
        trimmed.pop()
    while trimmed and not trimmed[0]:
        trimmed.pop(0)
    # Return a single string:
    return '\n'.join(trimmed)
