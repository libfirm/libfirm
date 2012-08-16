#!/usr/bin/env python
import sys
from jinja2 import Environment, Template
from spec_util import is_dynamic_pinned, isAbstract, load_spec
from filters import format_arguments, filter_isnot, filter_hasnot, filter_notset

def error(msg):
	"""writes an error message to stderr"""
	sys.stderr.write("Error: " + msg + "\n");

def warning(msg):
	"""writes a warning message to stderr"""
	sys.stderr.write("Warning: " + msg + "\n");

def format_args(arglist):
	return "\n".join(arglist)

def format_block(node):
	if hasattr(node, "knownBlock"):
		if hasattr(node, "knownGraph"):
			return ""
		return "env->irg"
	else:
		return "block"

env = Environment()
env.filters['args']      = format_args
env.filters['block']     = format_block
env.filters['arguments'] = format_arguments
env.filters['isnot']     = filter_isnot
env.filters['notset']    = filter_notset
env.filters['hasnot']    = filter_hasnot

def get_io_type(type, attrname, node):
	if type == "ir_tarval*":
		importcmd = "read_tarval(env)"
		exportcmd = "write_tarval(env, %(val)s);"
	elif type == "ir_mode*":
		importcmd = "read_mode_ref(env)"
		exportcmd = "write_mode_ref(env, %(val)s);"
	elif type == "ir_entity*":
		importcmd = "read_entity_ref(env)"
		exportcmd = "write_entity_ref(env, %(val)s);"
	elif type == "ir_type*":
		importcmd = "read_type_ref(env)"
		exportcmd = "write_type_ref(env, %(val)s);"
	elif type == "long":
		importcmd = "read_long(env)"
		exportcmd = "write_long(env, %(val)s);"
	elif type == "ir_relation":
		importcmd = "read_relation(env)"
		exportcmd = "write_relation(env, %(val)s);"
	elif type == "ir_where_alloc":
		importcmd = "read_where_alloc(env)"
		exportcmd = "write_where_alloc(env, %(val)s);"
	elif type == "ir_align":
		importcmd = "read_align(env)"
		exportcmd = "write_align(env, %(val)s);"
	elif type == "ir_volatility":
		importcmd = "read_volatility(env)"
		exportcmd = "write_volatility(env, %(val)s);"
	elif type == "ir_cons_flags":
		importcmd = "cons_none"
		exportcmd = "" # can't really export cons_flags
	elif type == "op_pin_state":
		importcmd = "read_pin_state(env)"
		exportcmd = "write_pin_state(env, node);"
	elif type == "ir_builtin_kind":
		importcmd = "read_builtin_kind(env)"
		exportcmd = "write_builtin_kind(env, node);"
	elif type == "cond_kind":
		importcmd = "read_cond_kind(env)"
		exportcmd = "write_cond_kind(env, node);"
	elif type == "cond_jmp_predicate":
		importcmd = "read_cond_jmp_predicate(env)"
		exportcmd = "write_cond_jmp_predicate(env, node);"
	elif type == "int":
		importcmd = "read_int(env)"
		exportcmd = "write_int(env, %(val)s);"
	elif type == "unsigned":
		importcmd = "read_unsigned(env)"
		exportcmd = "write_unsigned(env, %(val)s);"
	elif type == "long":
		importcmd = "read_long(env)"
		exportcmd = "write_long(env, %(val)s);"
	elif type == "ir_switch_table*":
		importcmd = "read_switch_table(env)"
		exportcmd = "write_switch_table(env, %(val)s);"
	else:
		warning("cannot generate import/export for node %s: unsupported attribute type: %s" % (node.name, type))
		importcmd = "/* BAD: %s %s */ (%s)0" % (type, attrname, type)
		exportcmd = "// BAD: %s" % type
	return (importcmd, exportcmd)

def prepare_attr(node, attr):
	(importcmd,exportcmd) = get_io_type(attr["type"], attr["name"], node)
	attr["importcmd"] = importcmd
	attr["exportcmd"] = exportcmd % {"val": "get_%s_%s(node)" % (node.name, attr["name"])}


def preprocess_node(node):
	if node.customSerializer:
		return

	# construct node arguments
	arguments = [ ]
	extraattrs = [ ]
	for input in node.ins:
		arguments.append("in_%s" % input[0])

	if node.arity == "variable" or node.arity == "dynamic":
		arguments.append("n_preds")
		arguments.append("preds")

	if not hasattr(node, "mode"):
		arguments.append("mode")

	for attr in node.attrs:
		prepare_attr(node, attr)
		if "to_flags" in attr:
			node.constructorFlags = True
			attr['to_flags'] = attr['to_flags'] % (attr["name"])
		elif "init" in attr:
			extraattrs.append(attr)
		else:
			arguments.append(attr["name"])

	for arg in node.constructor_args:
		if arg['type'] != "ir_cons_flags" and arg['name'] != "flags":
			error("only ir_cons_flags constructor arg supported in irio")
			continue
		node.constructorFlags = True
		arguments.append("flags")

	node.arguments  = arguments
	node.extraattrs = extraattrs
	node.dynamic_pinned = is_dynamic_pinned(node)

io_template = env.from_string('''/* Warning: automatically generated code */
{%- for node in nodes|notset('customSerializer') %}
static ir_node *read_{{node.name}}(read_env_t *env)
{
	{%- if not node.knownBlock %}
	ir_node *block = read_node_ref(env);
	{%- endif %}
	{%- for input in node.ins %}
	ir_node *in_{{input[0]}} = read_node_ref(env);
	{%- endfor %}
	{%- if not hasattr(node, "mode") %}
	ir_mode *mode = read_mode_ref(env);
	{%- endif %}
	{%- for attr in node.attrs %}
	{{attr.type}} {{attr.name}} = {{attr.importcmd}};
	{%- endfor %}
	{%- if node.dynamic_pinned %}
	op_pin_state pin_state = read_pin_state(env);
	{%- endif %}
	{%- if "fragile" in node.flags %}
	bool throws = read_throws(env);
	{%- endif %}
	{%- if node.arity == "dynamic" or node.arity == "variable" %}
	int       n_preds = read_preds(env);
	ir_node **preds   = (ir_node**)obstack_finish(&env->preds_obst);
	{%- endif %}
	{%- if node.constructorFlags %}
	ir_cons_flags flags = cons_none;
	{%- endif %}
	ir_node *res;
	{%- if node.constructorFlags %}
		{%- for attr in node.attrs %}
			{%- if "to_flags" in attr %}
	flags |= {{attr.to_flags}};
			{%- endif %}
		{%- endfor %}
		{%- if node.dynamic_pinned %}
	flags |= pin_state == op_pin_state_floats ? cons_floats : cons_none;
		{%- endif %}
		{%- if "fragile" in node.flags %}
	flags |= throws ? cons_throws_exception : cons_none;
		{%- endif %}
	{%- endif %}
	res = new_r_{{node.name}}(
		{%- filter arguments %}
{{node|block}}
{{node.arguments|args}}
		{%- if node.dynamic_pinned and not hasattr(node, "pinned_init") %}
pin_state
		{%- endif %}
{% endfilter %});

	{%- if node.arity == "dynamic" or node.arity == "variable" %}
	obstack_free(&env->preds_obst, preds);
	{%- endif %}
	{%- for attr in node.extraattrs %}
	set_{{node.name}}_{{attr.name}}(res, {{attr.name}});
	{%- endfor %}
	{%- if not node.constructorFlags %}
		{%- if node.dynamic_pinned and hasattr(node, "pinned_init") %}
	set_irn_pinned(res, pin_state);
		{%- endif %}
		{%- if "fragile" in node.flags and hasattr(node, "throws_init") %}
	ir_set_throws_exception(res, throws);
		{%- endif %}
	{%- endif %}
	return res;
}
{% endfor %}

{%- for node in nodes|notset('customSerializer') %}
static void write_{{node.name}}(write_env_t *env, const ir_node *node)
{
	write_symbol(env, "{{node.name}}");
	write_node_nr(env, node);
	{%- if not node.knownBlock %}
	write_node_ref(env, get_nodes_block(node));
	{%- endif %}
	{%- for input in node.ins %}
	write_node_ref(env, get_{{node.name}}_{{input[0]}}(node));
	{%- endfor %}
	{%- if not hasattr(node, "mode") %}
	write_mode_ref(env, get_irn_mode(node));
	{%- endif %}
	{%- for attr in node.attrs %}
	{{attr.exportcmd}}
	{%- endfor %}
	{%- if node.dynamic_pinned %}
	write_pin_state(env, get_irn_pinned(node));
	{%- endif %}
	{%- if "fragile" in node.flags %}
	write_throws(env, ir_throws_exception(node));
	{%- endif %}
	{%- if node.arity == "dynamic" or node.arity == "variable" %}
	write_pred_refs(env, node, {% if node.ins %}n_{{node.name}}_max+1{% else %}0{%endif%});
	{%- endif %}
}
{% endfor %}

static void register_generated_node_readers(void)
{
	{%- for node in nodes|notset('customSerializer') %}
	register_node_reader(new_id_from_str("{{node.name}}"), read_{{node.name}});
	{%- endfor %}
}

static void register_generated_node_writers(void)
{
	{%- for node in nodes|notset('customSerializer') %}
	register_node_writer(op_{{node.name}}, write_{{node.name}});
	{%- endfor %}
}
''')

def main(argv):
	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	specfile = argv[1]
	gendir = argv[2]

	spec = load_spec(specfile)
	nodes = spec.nodes
	real_nodes = []
	for node in nodes:
		if isAbstract(node):
			continue
		preprocess_node(node)
		real_nodes.append(node)

	file = open(gendir + "/gen_irio.inl", "w");
	file.write(io_template.render(nodes = real_nodes, hasattr=hasattr))
	file.close()

main(sys.argv)
