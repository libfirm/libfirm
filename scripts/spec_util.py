abstracts = set()
def abstract(cls):
	abstracts.add(cls)
	return cls
def isAbstract(nodetype):
	return nodetype in abstracts

def is_dynamic_pinned(node):
	return node.pinned in ["memory", "exception"]

def is_fragile(node):
	return hasattr(node, "flags") and "fragile" in node.flags

def inout_contains(l, name):
	for entry in l:
		if entry[0] == name:
			return True
	return False

def verify_node(node):
	if not hasattr(node, "pinned"):
		print "%s: NO PINNED SET" % node.__name__
	elif node.pinned not in ["yes", "no", "memory", "exception"]:
		print "%s: UNKNOWN PINNED MODE: %s" % (node.__name__, node.pinned)

	if not hasattr(node, "flags") and not isAbstract(node):
		print "WARNING: no flags specified for %s\n" % node.__name__
	elif type(node.flags) != list:
	 	print "ERROR: flags of %s not a list" % node.__name__
	if hasattr(node, "pinned_init") and not is_dynamic_pinned(node):
		print "ERROR: node %s has pinned_init attribute but is not marked as dynamically pinned" % node.__name__
	if is_fragile(node):
		if not is_dynamic_pinned(node):
			print "ERROR: fragile node %s must be dynamically pinned" % node.__name__
		if not hasattr(node, "throws_init"):
			print "ERROR: fragile node %s needs a throws_init attribute" % node.__name__
		if not inout_contains(node.ins, "mem"):
			print "ERROR: fragile node %s needs an input named 'mem'" % node.__name__
		if not inout_contains(node.outs, "X_regular"):
			print "ERROR: fragile node %s needs an output named 'X_regular'" % node.__name__
		if not inout_contains(node.outs, "X_except"):
			print "ERROR: fragile node %s needs an output named 'X_except'" % node.__name__
	else:
		if hasattr(node, "throws_init"):
			print "ERROR: throws_init only makes sense for fragile nodes"


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
	setdefault(node, "customSerializer", False)
	if hasattr(node, "outs"):
		node.mode = "mode_T"
