#!/usr/bin/env python
#
# This file is part of libFirm.
# Copyright (C) 2012 Karlsruhe Institute of Technology
import sys
import docutils.core
import docutils.writers.html4css1
import os.path
from datetime import datetime
from jinja2 import Environment, Template, FileSystemLoader
from spec_util import isAbstract, load_spec

tags = None
linkbase = None

def format_doxygrouplink(string, link=None):
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

def format_doxylink(string, link=None):
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

def format_docutils(string):
	writer = docutils.writers.html4css1.Writer()
	document = docutils.core.publish_parts(string, writer=writer)['body']
	return document

def prepare_nodes(nodes):
	real_nodes = []
	for node in nodes:
		if isAbstract(node):
			continue
		real_nodes.append(node)

	return real_nodes

def parse_tagfile(filename):
	global tags
	tagfile = open(filename)
	try:
		from lxml import etree
		tags = etree.parse(tagfile)
	except:
		tags = None

def main(argv):
	output = sys.stdout
	if len(argv) < 3:
		sys.stderr.write("usage: %s specfile templatefile [linkbase doxygen-tag-file]\n" % argv[0])
		sys.exit(1)
	specfile = argv[1]
	templatefile = argv[2]
	if len(argv) == 5:
		sys.stderr.write("loading tagfile\n")
		parse_tagfile(argv[3])
		global linkbase
		linkbase = argv[4]
		if linkbase != "":
			linkbase += "/"

	spec = load_spec(specfile)
	prepared_nodes = prepare_nodes(spec.nodes)

	basedir = os.path.dirname(templatefile)
	if basedir == "":
		basedir = "."
	loader = FileSystemLoader(basedir)
	env = Environment(loader=loader)
	env.filters['docutils'] = format_docutils
	env.filters['doxylink'] = format_doxylink
	env.filters['doxygrouplink'] = format_doxygrouplink
	env.globals['nodes'] = prepared_nodes
	env.globals['time'] = datetime.now().replace(microsecond=0).isoformat(' ')
	template = env.get_template(os.path.basename(templatefile))

	output.write(template.render())

main(sys.argv)
