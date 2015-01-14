# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import re

def filtjoin(string, joinstring):
	args = re.split('\s*\n\s*', string)
	if args[0] == '':
		args = args[1:]
	if len(args) > 0 and args[-1] == '':
		args = args[:-1]
	return joinstring.join(args)

def arguments(string, voidwhenempty = False):
	joined = filtjoin(string, ", ")
	if joined == "" and voidwhenempty:
		return "void"
	return joined

def hasnot(plist, flag):
	return list(filter(lambda x: not hasattr(x, flag) or not getattr(x, flag), plist))

def has(plist, flag):
	return list(filter(lambda x: hasattr(x, flag) and getattr(x, flag), plist))
