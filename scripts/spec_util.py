abstracts = set()
def abstract(cls):
	abstracts.add(cls)
	return cls
def isAbstract(nodetype):
	return nodetype in abstracts

def is_dynamic_pinned(node):
	return node.pinned in ["memory", "exception"]

def verify_node(node):
	if not hasattr(node, "pinned"):
		print "%s: NO PINNED SET" % node.__name__
	elif node.pinned not in ["yes", "no", "memory", "exception"]:
		print "%s: UNKNOWN PINNED MODE: %s" % (node.__name__, node.pinned)

	if not hasattr(node, "flags") and not isAbstract(node):
		print "WARNING: no flags specified for %s\n" % node.__name__
	elif type(node.flags) != list:
	 	print "ERROR: flags of %s not a list" % node.__name__

def setldefault(node, attr, val):
	# Don't use hasattr, as these things should not be inherited
	if attr not in node.__dict__:
		setattr(node, attr, val)

def setdefault(node, attr, val):
	# Don't use hasattr, as these things should not be inherited
	if not hasattr(node, attr):
		setattr(node, attr, val)

def setnodedefaults(node):
	setldefault(node, "name", node.__name__)
	if isAbstract(node):
		return

	setdefault(node, "ins", [])
	setdefault(node, "arity", len(node.ins))
	setdefault(node, "attrs", [])
	setdefault(node, "constructor_args", [])
	setldefault(node, "constrname", node.name)
	if hasattr(node, "outs"):
		node.mode = "mode_T"
