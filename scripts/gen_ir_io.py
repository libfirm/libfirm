#!/usr/bin/env python
import sys
from jinja2 import Environment, Template
import ir_spec

def format_args(arglist):
	#argstrings = map(lambda arg : arg["name"], arglist)
	#return ", ".join(argstrings)
	s = ", ".join(arglist)
	if len(s) == 0:
	  return "";
	return ", " + s;

def format_ifnset(string, node, key):
	if key in node:
		return ""
	return string

def format_block(node):
	if node.get("knownBlock"):
		return ""
	else:
		return ", get_node(env, preds[0])"

env = Environment()
env.filters['args']   = format_args
env.filters['ifnset'] = format_ifnset
env.filters['block']  = format_block

def get_io_type(type, attrname, nodename):
	if type == "tarval*":
		importcmd = "tarval *%s = read_tv(env);" % attrname
		exportcmd = "write_tarval(env, %(val)s);";
	elif type == "ir_mode*":
		importcmd = "ir_mode *%s = read_mode(env);" % attrname
		exportcmd = "write_mode(env, %(val)s);"
	elif type == "ir_entity*":
		importcmd = "ir_entity *%s = read_entity(env);" % attrname
		exportcmd = """fprintf(env->file, "%%ld ", get_entity_nr(%(val)s));"""
	elif type == "ir_type*":
		importcmd = "ir_type *%s = read_type(env);" % attrname
		exportcmd = """fprintf(env->file, "%%ld ", get_type_nr(%(val)s));"""
	elif type == "long" and nodename == "Proj":
		importcmd = "long %s = read_long(env);" % attrname
		exportcmd = """fprintf(env->file, "%%ld ", %(val)s);"""
	elif type == "pn_Cmp" or type == "ir_where_alloc":
		importcmd = "%s %s = (%s) read_long(env);" % (type, attrname, type)
		exportcmd = """fprintf(env->file, "%%ld ", (long) %(val)s);"""
	elif type == "ir_cons_flags" and nodename == "Store":
		importcmd = "ir_cons_flags %s = get_cons_flags(env);" % attrname
		exportcmd = """write_pin_state(env, irn);
			write_volatility(env, irn);
			write_align(env, irn);"""
	elif type == "ir_cons_flags" and nodename == "Load":
		importcmd = "ir_cons_flags %s = get_cons_flags(env);" % attrname
		exportcmd = """write_pin_state(env, irn);
			write_volatility(env, irn);
			write_align(env, irn);"""
	elif type == "op_pin_state":
		importcmd = "op_pin_state %s = read_pin_state(env);" % attrname
		exportcmd = "write_pin_state(env, irn);"
	elif type == "ir_builtin_kind":
		importcmd = "ir_builtin_kind %s = read_builtin_kind(env);" % attrname
		exportcmd = "write_builtin_kind(env, irn);"
	elif type == "cond_kind":
		importcmd = "cond_kind %s = read_cond_kind(env);" % attrname
		exportcmd = "write_cond_kind(env, irn);"
	elif type == "cond_jmp_predicate":
		importcmd = "cond_jmp_predicate %s = read_cond_jmp_predicate(env);" % attrname
		exportcmd = "write_cond_jmp_predicate(env, irn);"
	elif type == "int":
		importcmd = "int %s = (int) read_long(env);" % attrname
		exportcmd = """fprintf(env->file, "%%d ", %(val)s);"""
	elif type == "long":
		importcmd = "long %s = read_long(env);" % attrname
		exportcmd = """fprintf(env->file, "%%ld ", %(val)s);"""
	else:
		print "UNKNOWN TYPE: %s" % type
		importcmd = """// BAD: %s %s
			%s %s = (%s) 0;""" % (type, attrname, type, attrname, type)
		exportcmd = "// BAD: %s" % type
	return (importcmd, exportcmd)

"""	if type == "ir_type*":
		java_type    = "firm.Type"
		wrap_type    = "Pointer"
		to_wrapper   = "%s.ptr"
		from_wrapper = "firm.Type.createWrapper(%s)"
	elif type == "ir_mode*":
		java_type    = "firm.Mode"
		wrap_type    = "Pointer"
		to_wrapper   = "%s.ptr"
		from_wrapper = "new firm.Mode(%s)"
	elif type == "tarval*":
		java_type    = "firm.TargetValue"
		wrap_type    = "Pointer"
		to_wrapper   = "%s.ptr"
		from_wrapper = "new firm.TargetValue(%s)"
	elif type == "pn_Cmp":
		java_type    = "int"
		wrap_type    = "int"
		to_wrapper   = "%s"
		from_wrapper = "%s"
	elif type == "long":
		java_type    = "int"
		wrap_type    = "com.sun.jna.NativeLong"
		to_wrapper   = "new com.sun.jna.NativeLong(%s)"
		from_wrapper = "%s.intValue()"
	elif type == "cons_flags":
		java_type    = "firm.bindings.binding_ircons.ir_cons_flags"
		wrap_type    = "int"
		to_wrapper   = "%s.val"
		from_wrapper = "firm.bindings.binding_ircons.ir_cons_flags.getEnum(%s)"
	elif type == "ir_where_alloc":
		java_type    = "firm.bindings.binding_ircons.ir_where_alloc"
		wrap_type    = "int"
		to_wrapper   = "%s.val"
		from_wrapper = "firm.bindings.binding_ircons.ir_where_alloc.getEnum(%s)"
	elif type == "ir_entity*":
		java_type    = "firm.Entity"
		wrap_type    = "Pointer"
		to_wrapper   = "%s.ptr"
		from_wrapper = "new firm.Entity(%s)"
	else:
		print "UNKNOWN TYPE"
		java_type    = "BAD"
		wrap_type    = "BAD"
		to_wrapper   = "BAD"
		from_wrapper = "BAD"
	return (java_type,wrap_type,to_wrapper,from_wrapper)"""

def prepare_attr(nodename, attr):
	(importcmd,exportcmd) = get_io_type(attr["type"], attr["name"], nodename)
	attr["importcmd"] = importcmd
	attr["exportcmd"] = exportcmd % {"val": "get_%s_%s(irn)" % (nodename, attr["name"])}

def preprocess_node(nodename, node):
	if "is_a" in node:
		parent = ir_spec.nodes[node["is_a"]]
		node["ins"] = parent["ins"]
		if "outs" in parent:
			node["outs"] = parent["outs"]
	if "ins" not in node:
		node["ins"] = []
	if "outs" in node:
		node["mode"] = "mode_T"
	if "arity" not in node:
		node["arity"] = len(node["ins"])
	if "attrs" not in node:
		node["attrs"] = []
	if "constructor_args" not in node:
		node["constructor_args"] = []

	# construct node arguments
	arguments = [ ]
	initargs = [ ]
	specialconstrs = [ ]
	i = 0
	for input in node["ins"]:
		arguments.append("prednodes[%i]" % i)
		i += 1

	# Special case for Builtin...
	if nodename == "Builtin":
		for attr in node["attrs"]:
			if attr["name"] == "kind":
				prepare_attr(nodename, attr)
				arguments.append(attr["name"])

	if node["arity"] == "variable" or node["arity"] == "dynamic":
		arguments.append("numpreds - %i" % (i + 1))
		arguments.append("prednodes + %i" % i)

	if "mode" not in node:
		arguments.append("mode")

	attrs_with_special = 0
	for attr in node["attrs"]:
		if nodename == "Builtin" and attr["name"] == "kind":
			continue
		prepare_attr(nodename, attr)
		if "special" in attr:
			if not "init" in attr:
				print "Node type %s has an attribute with a \"special\" entry but without \"init\"" % nodename
				sys.exit(1)

			if attrs_with_special != 0:
				print "Node type %s has more than one attribute with a \"special\" entry" % nodename
				sys.exit(1)

			attrs_with_special += 1

			if "prefix" in attr["special"]:
				specialname = attr["special"]["prefix"] + nodename
			elif "suffix" in attr["special"]:
				specialname = nodename + attr["special"]["suffix"]
			else:
				print "Unknown special constructor type for node type %s" %nodename
				sys.exit(1)

			specialconstrs.append(
				dict(
					constrname = specialname,
					attrname = attr["name"],
					value = attr["special"]["init"]
				)
			)
		elif "init" in attr:
			initargs.append(attr["name"])
		else:
			arguments.append(attr["name"])

	for arg in node["constructor_args"]:
		prepare_attr(nodename, arg)
		arguments.append(arg["name"])

	node["arguments"] = arguments
	node["initargs"] = initargs
	node["special_constructors"] = specialconstrs

export_attrs_template = env.from_string('''
	case iro_{{nodename}}:
		{{"write_mode(env, get_irn_mode(irn));"|ifnset(node,"mode")}}
		{% for attr in node.attrs %}{{attr.exportcmd}}
		{% endfor %}
		{% for attr in node.constructor_args %}{{attr.exportcmd}}
		{% endfor %}break;''')

import_attrs_template = env.from_string('''
	case iro_{{nodename}}:
	{
		{{"ir_mode *mode = read_mode(env);"|ifnset(node,"mode")}}
		{% for attr in node.attrs %}{{attr.importcmd}}
		{% endfor %}
		{% for attr in node.constructor_args %}{{attr.importcmd}}
		{% endfor %}
		{% for special in node.special_constructors %}if({{special.attrname}} == {{special.value}})
			newnode = new_r_{{special.constrname}}(current_ir_graph{{node|block}}{{node["arguments"]|args}});
		else{% endfor %}
		newnode = new_r_{{nodename}}(current_ir_graph{{node|block}}{{node["arguments"]|args}});
		{% for initarg in node.initargs %}set_{{nodename}}_{{initarg}}(newnode, {{initarg}});
		{% endfor %}
		break;
	}
''')

def main(argv):
	"""the main function"""

	if len(argv) < 3:
		print "usage: %s specname(ignored) destdirectory" % argv[0]
		sys.exit(1)

	gendir = argv[2]

	file = open(gendir + "/gen_irio_export.inl", "w");
	for nodename, node in ir_spec.nodes.iteritems():
		preprocess_node(nodename, node)
		if not "abstract" in node:
			file.write(export_attrs_template.render(vars()))
	file.write("\n")
	file.close()

	file = open(gendir + "/gen_irio_import.inl", "w");
	for nodename, node in ir_spec.nodes.iteritems():
		if not "abstract" in node and nodename != "Start" and nodename != "End" and nodename != "Anchor" and nodename != "SymConst" and nodename != "Block":
			file.write(import_attrs_template.render(vars()))
	# TODO: SymConst
	file.write("\n")
	file.close()

	file = open(gendir + "/gen_irio_lex.inl", "w");
	for nodename, node in ir_spec.nodes.iteritems():
		if not "abstract" in node:
			file.write("\tINSERT(\"" + nodename + "\", tt_iro, iro_" + nodename + ");\n");
	file.close()

if __name__ == "__main__":
	main(sys.argv)
