# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import re

def format_arguments(string, voidwhenempty = False):
	args = re.split('\s*\n\s*', string)
	if args[0] == '':
		args = args[1:]
	if len(args) > 0 and args[-1] == '':
		args = args[:-1]
	if len(args) == 0 and voidwhenempty:
		return "void"
	return ", ".join(args)

def filter_hasnot(list, flag):
	return filter(lambda x: not hasattr(x, flag) or not getattr(x, flag), list)

def filter_has(list, flag):
	return filter(lambda x: hasattr(x, flag) and getattr(x, flag), list)
