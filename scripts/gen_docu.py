#!/usr/bin/env python
import sys
import re
import docutils.core
import docutils.writers.html4css1
from datetime import datetime
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
from spec_util import is_dynamic_pinned, verify_node, isAbstract, setdefault, trim_docstring
from ir_spec import nodes

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
	e = tags.xpath("//member[name/text()='%s']" % link)
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

env = Environment()
env.filters['docutils'] = format_docutils
env.filters['doxylink'] = format_doxylink
env.filters['doxygrouplink'] = format_doxygrouplink

docu_template = env.from_string(
'''<html>
	<head>
		<title>libFirm node specifications</title>
		<link rel='stylesheet' type='text/css' href='style.css'/>
	</head>
	<body>
		<div class="document">
		<div class="documentwrapper">
			<div class="bodywrapper"><div class="body">
				<h1>Firm Node Types</h1>
				{% for node in nodes %}
				<div class="section" id="{{node.name}}">
					<h3>{{node.name}}</h3>
					{{node.doc|docutils}}
					<h5>Inputs</h5>
					<dl>
					{% for input in node.ins %}
						<dt>{{input[0]}}</dt><dd>{{input[1]}}</dd>
					{% endfor %}
					{% if node.arity == "variable" %}
						<dt>...</dt><dd>additional inputs (oparity_variable)</dd>
					{% elif node.arity == "dynamic" %}
						<dt>...</dt><dd>inputs dynamically mananged (oparity_dynamic)</dd>
					{% endif %}
					</dl>
					{% if node.outs %}
					<h5>Outputs</h5>
					<dl>
					{% for output in node.outs %}
						<dt>{{output[0]}}</dt><dd>{{output[1]}}</dd>
					{% endfor %}
					</dl>
					{% endif %}
					{% if node.attrs %}
					<h5>Attributes</h5>
					<dl>
					{% for attr in node.attrs %}
						<dt>{{attr.name}}</dt><dd>{{attr.comment}} ({{attr.type}})</dd>
					{% endfor %}
					{% endif %}
					</dl>
					{% set comma = joiner(", ") %}
					<h5>Flags</h5>
					{% for flag in node.flags -%}
						{{comma()}}{{flag|doxylink("irop_flag_" + flag)}}
					{%- endfor %}
					<h5>{{"API"|doxygrouplink(node.name)}}</h5>
					<hr/>
				</div>
				{% endfor %}
			</div></div>
		</div>
		<div class="sidebar">
			<div class="sidebarwrapper">
				<h3>Table Of Contents</h3>
				<ul>
					<li><a href="#">Firm Node Types</a>
					<ul>
						{% for node in nodes %}
						<li><a href="#{{node.name}}">{{node.name}}</a></li>
						{% endfor %}
					</ul>
					</li>
			</div>
		</div>
		</div>
		<div class="footer">
			Generated {{time}}
		</div>
	</body>
</html>
''')

#############################

def preprocess_node(node):
	node.doc = trim_docstring(node.__doc__)

def prepare_nodes():
	real_nodes = []
	for node in nodes:
		preprocess_node(node)
		if isAbstract(node):
			continue
		real_nodes.append(node)

	return real_nodes

def main(argv):
	global tags
	output = sys.stdout
	if len(argv) > 1:
		output = open(argv[-1], "w")
	if len(argv) > 3:
		tagfile = open(argv[-3], "r")
		global linkbase
		linkbase = argv[-2]
		if linkbase != "":
			linkbase += "/"
		try:
			from lxml import etree
			tags = etree.parse(tagfile)
		except:
			tags = None

	real_nodes = prepare_nodes()
	time = datetime.now().replace(microsecond=0).isoformat(' ')
	output.write(docu_template.render(nodes=real_nodes, time=time))
	if output != sys.stdout:
		output.close()

main(sys.argv)
