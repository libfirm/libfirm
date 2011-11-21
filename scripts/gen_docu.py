#!/usr/bin/env python
import sys
import re
import docutils.core
import docutils.writers.html4css1
from datetime import datetime
from jinja2 import Environment, Template
from jinja2.filters import do_dictsort
from spec_util import is_dynamic_pinned, verify_node, isAbstract, setdefault
from ir_spec import nodes

def trim(docstring):
    if not docstring:
        return ''
    # Convert tabs to spaces (following the normal Python rules)
    # and split into a list of lines:
    lines = docstring.expandtabs().splitlines()
    # Determine minimum indentation (first line doesn't count):
    indent = sys.maxint
    for line in lines[1:]:
        stripped = line.lstrip()
        if stripped:
            indent = min(indent, len(line) - len(stripped))
    # Remove indentation (first line is special):
    trimmed = [lines[0].strip()]
    if indent < sys.maxint:
        for line in lines[1:]:
            trimmed.append(line[indent:].rstrip())
    # Strip off trailing and leading blank lines:
    while trimmed and not trimmed[-1]:
        trimmed.pop()
    while trimmed and not trimmed[0]:
        trimmed.pop(0)
    # Return a single string:
    return '\n'.join(trimmed)

def format_docutils(string):
	writer = docutils.writers.html4css1.Writer()
	document = docutils.core.publish_parts(string, writer=writer)['body']
	return document

env = Environment()
env.filters['docutils'] = format_docutils

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
					<h5>Flags</h5>
					{% for flag in node.flags %} {{flag}} {% endfor %}
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
	node.doc = trim(node.__doc__)

def prepare_nodes():
	real_nodes = []
	for node in nodes:
		preprocess_node(node)
		if isAbstract(node):
			continue
		real_nodes.append(node)

	return real_nodes

def main(argv):
	real_nodes = prepare_nodes()
	time = datetime.now().replace(microsecond=0).isoformat(' ')
	sys.stdout.write(docu_template.render(nodes = real_nodes, time=time))

main(sys.argv)
