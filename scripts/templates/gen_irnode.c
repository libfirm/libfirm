{{warning}}
#include <assert.h>
{% if spec.external %}
#include "gen_irnode.h"
#include <libfirm/firm_types.h>
#include <libfirm/irnode.h>
#include <libfirm/irop.h>
#include <libfirm/irgopt.h>
#include <libfirm/ircons.h>
#include <libfirm/irverify.h>
#include <libfirm/irgraph.h>
#include "{{spec.external}}/nodes.h"
{% else %}
#include "irnode_t.h"
#include "irop_t.h"
#include "irverify_t.h"
#include "iropt_t.h"
#include "ircons_t.h"
#include "irgraph_t.h"
#include "irbackedge_t.h"
#include "irgopt.h"
#include "util.h"
{% endif %}

{% if spec.external %}
static unsigned {{spec.name}}_opcode_start;

int is_{{spec.name}}_node(const ir_node *node)
{
	unsigned start = {{spec.name}}_opcode_start;
	unsigned opc   = get_irn_opcode(node);
	return opc > start && opc - start <= {{spec.name}}o_last;
}

{{spec.name}}_opcode get_{{spec.name}}_irn_opcode(const ir_node *node)
{
	assert(is_{{spec.name}}_node(node));
	return ({{spec.name}}_opcode) (get_irn_opcode(node) - {{spec.name}}_opcode_start);
}
{% endif %}

{% for node in nodes %}

{%- if node.constructor %}
ir_node *new_rd_{{node.name}}(
	{%- filter parameters %}
		dbg_info *dbgi
		{{node|blockparameter}}
		{{node|nodeparameters}}
	{% endfilter %})
{
	{{node|blockassign}}
	{{node|irgassign}}
	{{node|insdecl}}

	ir_node *res = new_ir_node(
		{%- filter arguments %}
			dbgi
			irg
			block
			op_{{node.name}}
			{% if node.mode -%}
				{{node.mode}}
			{%- else -%}
				mode
			{%- endif %}
			{{node|arity_and_ins}}
		{% endfilter %});
	{% if len(node.attrs) + len(node.initattrs) > 0 -%}
	{% if spec.external -%}
	{{node.attr_struct}} *attr = ({{node.attr_struct}}*) get_irn_generic_attr(res);
	{%- else -%}
	{{node.attr_struct}} *attr = &res->attr.{{node.attrs_name}};
	{%- endif %}
	{%- for attr in node.attrs %}
	attr->{{attr.fqname}} =
		{%- if attr.init %} {{ attr.init -}};
		{%- else         %} {{ attr.name -}};
		{%- endif %}
	{%- endfor %}
	{%- for attr in node.initattrs %}
	attr->{{attr.fqname}} = {{ attr.init -}};
	{%- endfor %}
	{%- endif %}
	{{- node.init }}
	verify_new_node(res);
	res = optimize_node(res);
	{{- node.init_after_opt }}
	return res;
}

ir_node *new_r_{{node.name}}(
		{%- filter parameters %}
			{{node|blockparameter}}
			{{node|nodeparameters}}
		{% endfilter %})
{
	return new_rd_{{node.name}}(
		{%- filter arguments %}
			NULL
			{{node|blockargument}}
			{{node|nodearguments}}
		{% endfilter %});
}

ir_node *new_d_{{node.name}}(
		{%- filter parameters %}
			dbg_info *dbgi
			{{node|nodeparameters}}
		{% endfilter %})
{
	assert(irg_is_constrained(current_ir_graph, IR_GRAPH_CONSTRAINT_CONSTRUCTION));
	ir_node *res = new_rd_{{node.name}}(
		{%- filter parameters %}
			dbgi
			{{node|curblock}}
			{{node|nodearguments}}
		{% endfilter %});
	return res;
}

ir_node *new_{{node.name}}(
		{%- filter parameters %}
			{{node|nodeparameters}}
		{% endfilter %})
{
	return new_d_{{node.name}}(
		{%- filter arguments %}
			NULL
			{{node|nodearguments}}
		{% endfilter %});
}
{% endif %}

{%- for input in node.ins %}
ir_node *(get_{{node.name}}_{{input.name}})(const ir_node *node)
{
	return get_{{node.name}}_{{input.name}}(node);
}

void (set_{{node.name}}_{{input.name}})(ir_node *node, ir_node *{{input.name|escape_keywords}})
{
	set_{{node.name}}_{{input.name}}_(node, {{input.name|escape_keywords}});
}
{% endfor %}

{%- if node.input_name %}
int (get_{{node.name}}_n_{{node.input_name}}s)(ir_node const *node)
{
	return get_{{node.name}}_n_{{node.input_name}}s_(node);
}

ir_node *(get_{{node.name}}_{{node.input_name}})(ir_node const *node, int pos)
{
	return get_{{node.name}}_{{node.input_name}}_(node, pos);
}

void (set_{{node.name}}_{{node.input_name}})(ir_node *node, int pos, ir_node *{{node.input_name}})
{
	set_{{node.name}}_{{node.input_name}}_(node, pos, {{node.input_name}});
}

ir_node **(get_{{node.name}}_{{node.input_name}}_arr)(ir_node *node)
{
	return get_{{node.name}}_{{node.input_name}}_arr_(node);
}
{% endif -%}

ir_op *op_{{node.name}};
ir_op *get_op_{{node.name}}(void)
{
	return op_{{node.name}};
}
{% endfor %}

{%- for node in nodes+abstract_nodes %}

int (is_{{node.name}})(const ir_node *node)
{
	return is_{{node.name}}_(node);
}
{%- for attr in node.attrs|hasnot("noprop") %}

{{attr.type}} (get_{{node.name}}_{{attr.name}})(const ir_node *node)
{
	return get_{{node.name}}_{{attr.name}}_(node);
}

void (set_{{node.name}}_{{attr.name}})(ir_node *node, {{attr.type}} {{attr.name}})
{
	set_{{node.name}}_{{attr.name}}_(node, {{attr.name}});
}
{%- endfor -%}
{% endfor %}

void {{spec.name}}_init_opcodes(void)
{
	{%- if spec.external %}
	{{spec.name}}_opcode_start = get_next_ir_opcodes({{spec.name}}o_last+1);
	unsigned o = {{spec.name}}_opcode_start;
	{% endif -%}

	{%- for node in nodes %}
	op_{{node.name}} = new_ir_op(
		{%- filter arguments %}
			{%- if spec.external -%} o+ {%- endif -%}
			{{spec.name}}o_{{node.name}}
			"{{node.name}}"
			{{node|pinned}}
			{{node|flags}}
			{{node|arity}}
			{{node|opindex}}
			{{node|attr_size}}
		{% endfilter %});
	{%- if "uses_memory" in node.flags: %}
	ir_op_set_memory_index(op_{{node.name}}, n_{{node.name}}_mem);
	{%- endif -%}
	{%- if "fragile" in node.flags: %}
	ir_op_set_fragile_indices(op_{{node.name}}, pn_{{node.name}}_X_regular, {% if node.only_regular -%} (unsigned)-1 {%- else -%} pn_{{node.name}}_X_except {%- endif -%});
	{%- endif -%}
	{%- endfor %}
}

void {{spec.name}}_finish_opcodes(void)
{
	{%- for node in nodes %}
	free_ir_op(op_{{node.name}}); op_{{node.name}} = NULL;
	{%- endfor %}
}
