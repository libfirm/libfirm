#!/usr/bin/env python
#
# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology.
import sys
import argparse
from jinja2 import Environment, Template
from irops import prepare_nodes, load_spec
import filters
import irops
import jinjautil

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

	loader = jinjautil.SimpleLoader()
	env = Environment(loader=loader, keep_trailing_newline=True)
	env.globals.update(jinjautil.exports)
	env.filters.update(jinjautil.filters)

	env.globals['nodes']          = nodes
	env.globals['abstract_nodes'] = abstract_nodes
	env.globals['spec']           = spec
	loader.includedirs += config.includedirs
	template = env.get_template(config.templatefile)
	result = template.render()
	sys.stdout.write(result)

if __name__ == "__main__":
	main(sys.argv)
