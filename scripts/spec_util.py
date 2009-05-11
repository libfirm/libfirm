def is_dynamic_pinned(node):
	return node["pinned"] in ["memory", "exception"]

def verify_node(nodename, node):
	if "pinned" not in node:
		print "%s: NO PINNED SET" % nodename
	elif node["pinned"] not in ["yes", "no", "memory", "exception"]:
		print "%s: UNKNOWN PINNED MODE: %s" % (nodename, node["pinned"])
