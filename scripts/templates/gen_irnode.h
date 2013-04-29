{{warning}}
{% for node in nodes %}
#define is_{{node.name}}(node) is_{{node.name}}_(node)
{%- for attr in node.attrs|hasnot("noprop") %}
#define get_{{node.name}}_{{attr.name}}(node) get_{{node.name}}_{{attr.name}}_(node)
#define set_{{node.name}}_{{attr.name}}(node, {{attr.name}}) set_{{node.name}}_{{attr.name}}_(node, {{attr.name}})
{%- endfor -%}
{%- for input in node.ins %}
#define get_{{node.name}}_{{input[0]}}(node) get_{{node.name}}_{{input[0]}}_(node)
#define set_{{node.name}}_{{input[0]}}(node, {{input[0]|escape_keywords}}) set_{{node.name}}_{{input[0]}}_(node, {{input[0]|escape_keywords}})
{%- endfor -%}
{%- if node.input_name %}
#define get_{{node.name}}_n_{{node.input_name}}s(node) get_{{node.name}}_n_{{node.input_name}}s_(node)
#define get_{{node.name}}_{{node.input_name}}(node, pos) get_{{node.name}}_{{node.input_name}}_(node, pos)
#define set_{{node.name}}_{{node.input_name}}(node, pos, {{node.input_name}}) set_{{node.name}}_{{node.input_name}}_(node, pos, {{node.input_name}})
#define get_{{node.name}}_{{node.input_name}}_arr(node) get_{{node.name}}_{{node.input_name}}_arr_(node)
{%- endif %}
{% endfor %}

{%- for node in nodes %}
static inline int is_{{node.name}}_(const ir_node *node)
{
	return get_irn_op(node) == op_{{node.name}};
}
{%  for attr in node.attrs|hasnot("noprop") %}
static inline {{attr.type}} get_{{node.name}}_{{attr.name}}_(const ir_node *node)
{
	assert(is_{{node.name}}(node));
	return node->attr.{{node.attrs_name}}.{{attr.name}};
}

static inline void set_{{node.name}}_{{attr.name}}_(ir_node *node, {{attr.type}} {{attr.name}})
{
	assert(is_{{node.name}}(node));
	node->attr.{{node.attrs_name}}.{{attr.name}} = {{attr.name}};
}
{% endfor -%}

{%- for input in node.ins %}
static inline ir_node *get_{{node.name}}_{{input[0]}}_(const ir_node *node)
{
	assert(is_{{node.name}}(node));
	return get_irn_n(node, n_{{node.name}}_{{input[0]}});
}

static inline void set_{{node.name}}_{{input[0]}}_(ir_node *node, ir_node *{{input[0]|escape_keywords}})
{
	assert(is_{{node.name}}(node));
	set_irn_n(node, n_{{node.name}}_{{input[0]}}, {{input[0]|escape_keywords}});
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
	return get_irn_in(node) + 1{% if node.ins %}+ (n_{{node.name}}_max + 1){% endif %};
}
{% endif -%}
{% endfor -%}
