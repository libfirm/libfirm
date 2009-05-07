def is_dynamic_pinned(node):
	return node["pinned"] in ["memory", "exception"]

def verify_node(node):
	if node["pinned"] not in ["yes", "no", "memory", "exception"]:
		print "UNKNOWN PINNED MODE: %s" % node["pinned"]
