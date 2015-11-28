{{warning}}
#ifndef {{spec.name|upper}}_GEN_IRNODE_H
#define {{spec.name|upper}}_GEN_IRNODE_H

#include <assert.h>
{% if spec.external %}
#include <libfirm/firm_types.h>
#include <libfirm/irnode.h>
#include <{{spec.external}}/nodes.h>
#include "nodes_attr.h"
{% endif %}

{% for node in nodes %}
#define is_{{node.name}}(node) is_{{node.name}}_(node)
{%- for attr in node.attrs|hasnot("noprop") %}
#define get_{{node.name}}_{{attr.name}}(node) get_{{node.name}}_{{attr.name}}_(node)
#define set_{{node.name}}_{{attr.name}}(node, {{attr.name}}) set_{{node.name}}_{{attr.name}}_(node, {{attr.name}})
{%- endfor -%}
{%- for input in node.ins %}
#define get_{{node.name}}_{{input.name}}(node) get_{{node.name}}_{{input.name}}_(node)
#define set_{{node.name}}_{{input.name}}(node, {{input.name|escape_keywords}}) set_{{node.name}}_{{input.name}}_(node, {{input.name|escape_keywords}})
{%- endfor -%}
{%- if node.input_name %}
#define get_{{node.name}}_n_{{node.input_name}}s(node) get_{{node.name}}_n_{{node.input_name}}s_(node)
#define get_{{node.name}}_{{node.input_name}}(node, pos) get_{{node.name}}_{{node.input_name}}_(node, pos)
#define set_{{node.name}}_{{node.input_name}}(node, pos, {{node.input_name}}) set_{{node.name}}_{{node.input_name}}_(node, pos, {{node.input_name}})
#define get_{{node.name}}_{{node.input_name}}_arr(node) get_{{node.name}}_{{node.input_name}}_arr_(node)
{%- endif %}
{% endfor %}
{% for node in abstract_nodes %}
{%- for attr in node.attrs|hasnot("noprop") %}
#define is_{{node.name}}(node) is_{{node.name}}_(node)
#define get_{{node.name}}_{{attr.name}}(node) get_{{node.name}}_{{attr.name}}_(node)
#define set_{{node.name}}_{{attr.name}}(node, {{attr.name}}) set_{{node.name}}_{{attr.name}}_(node, {{attr.name}})
{%- endfor -%}
{% endfor %}

{%- for node in nodes %}
static inline int is_{{node.name}}_(const ir_node *node)
{
	return get_irn_op(node) == op_{{node.name}};
}

{%- for input in node.ins %}
static inline ir_node *get_{{node.name}}_{{input.name}}_(const ir_node *node)
{
	assert(is_{{node.name}}(node));
	return get_irn_n(node, n_{{node.name}}_{{input.name}});
}

static inline void set_{{node.name}}_{{input.name}}_(ir_node *node, ir_node *{{input.name|escape_keywords}})
{
	assert(is_{{node.name}}(node));
	set_irn_n(node, n_{{node.name}}_{{input.name}}, {{input.name|escape_keywords}});
}
{% endfor %}

{%- if node.input_name %}
static inline int get_{{node.name}}_n_{{node.input_name}}s_(ir_node const *node)
{
	assert(is_{{node.name}}(node));
	return get_irn_arity(node){% if node.ins %} - (n_{{node.name}}_max + 1){% endif %};
}

static inline ir_node *get_{{node.name}}_{{node.input_name}}_(ir_node const *node, int pos)
{
	assert(0 <= pos && pos < get_{{node.name}}_n_{{node.input_name}}s(node));
	return get_irn_n(node, pos{% if node.ins %} + (n_{{node.name}}_max + 1){% endif %});
}

static inline void set_{{node.name}}_{{node.input_name}}_(ir_node *node, int pos, ir_node *{{node.input_name}})
{
	assert(0 <= pos && pos < get_{{node.name}}_n_{{node.input_name}}s(node));
	set_irn_n(node, pos{% if node.ins %} + (n_{{node.name}}_max + 1){% endif %}, {{node.input_name}});
}

static inline ir_node **get_{{node.name}}_{{node.input_name}}_arr_(ir_node *node)
{
	assert(is_{{node.name}}(node));
	return get_irn_in(node){% if node.ins %} + (n_{{node.name}}_max + 1){% endif %};
}
{% endif -%}
{% endfor -%}

{%- for node in abstract_nodes %}
{%- if node.name != "binop" %}
static inline int is_{{node.name}}_(const ir_node *node)
{
	return {% filter filtjoin(' || ') -%}
	{% for subc in node.__subclasses__() %}
		is_{{subc.name}}(node)
	{% endfor %}
	{%- endfilter %};
}

{% endif -%}
{% endfor -%}

{% for node in nodes+abstract_nodes %}

{%- for attr in node.attrs|hasnot("noprop") %}
static inline {{attr.type}} get_{{node.name}}_{{attr.name}}_(const ir_node *node)
{
	assert(is_{{node.name}}(node));
	{% if spec.external -%}
	{{node.attr_struct}} const *const attr = ({{node.attr_struct}} const*)get_irn_generic_attr_const(node);
	return attr->{{attr.name}};
	{%- else -%}
	return node->attr.{{node.attrs_name}}.{{attr.name}};
	{%- endif %}
}

static inline void set_{{node.name}}_{{attr.name}}_(ir_node *node, {{attr.type}} {{attr.name}})
{
	assert(is_{{node.name}}(node));
	{% if spec.external -%}
	{{node.attr_struct}} *attr = ({{node.attr_struct}}*)get_irn_generic_attr(node);
	attr->{{attr.name}} = {{attr.name}};
	{%- else -%}
	node->attr.{{node.attrs_name}}.{{attr.name}} = {{attr.name}};
	{%- endif %}
}
{% endfor -%}

{% endfor -%}

void {{spec.name}}_init_opcodes(void);
void {{spec.name}}_finish_opcodes(void);

#endif
