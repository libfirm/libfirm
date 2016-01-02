{{warning}}

#include "irio_t.h"

{%- for node in nodes|has('serializer') %}
static ir_node *read_{{node.name}}(read_env_t *env)
{
	{%- if not node.block %}
	ir_node *block = read_node_ref(env);
	{%- endif %}
	{%- for input in node.ins %}
	ir_node *irn_{{input.name}} = read_node_ref(env);
	{%- endfor %}
	{%- if not node.mode %}
	ir_mode *mode = read_mode_ref(env);
	{%- endif %}
	{%- for attr in node.attrs %}
	{{attr.type}} {{attr.name}} = read_{{attr.type|simplify_type}}(env);
	{%- endfor %}
	{%- if is_dynamic_pinned(node) %}
	int pinned = read_pinned(env);
	{%- endif %}
	{%- if "fragile" in node.flags %}
	bool throws = read_throws(env);
	{%- endif %}
	{%- if node.arity == "dynamic" or node.arity == "variable" %}
	int arity = read_preds(env);
	ir_node **in = (ir_node**)obstack_finish(&env->preds_obst);
	{%- endif %}
	{%- if node.attrs|has('to_flags') %}
	ir_cons_flags flags = cons_none
		{%- for attr in node.attrs %}
			{%- if attr.to_flags %}
		| ({{attr.to_flags|stringformat(attr.name)}})
			{%- endif %}
		{%- endfor %}
		{%- if is_dynamic_pinned(node) %}
		| (pinned == 0 ? cons_floats : cons_none)
		{%- endif %}
		{%- if "fragile" in node.flags %}
		| (throws ? cons_throws_exception : cons_none)
		{%- endif -%}
		;
	{%- endif %}
	ir_node *res = new_r_{{node.name}}(
		{%- filter arguments %}
			{{node|block}}
			{{node.arguments|args}}
		{%- endfilter %});

	{%- if node.arity == "dynamic" or node.arity == "variable" %}
	obstack_free(&env->preds_obst, in);
	{%- endif %}
	{%- for attr in node.attrs|has('init') %}
	set_{{node.name}}_{{attr.name}}(res, {{attr.name}});
	{%- endfor %}
	{%- if not node.constructorFlags %}
		{%- if is_dynamic_pinned(node) and hasattr(node, "pinned_init") %}
	set_irn_pinned(res, pinned);
		{%- endif %}
		{%- if "fragile" in node.flags and hasattr(node, "throws_init") %}
	ir_set_throws_exception(res, throws);
		{%- endif %}
	{%- endif %}
	return res;
}
{% endfor %}

{%- for node in nodes|has('serializer') %}
static void write_{{node.name}}(write_env_t *env, const ir_node *node)
{
	write_symbol(env, "{{node.name}}");
	write_node_nr(env, node);
	{%- if not node.block %}
	write_node_ref(env, get_nodes_block(node));
	{%- endif %}
	{%- for input in node.ins %}
	write_node_ref(env, get_{{node.name}}_{{input.name}}(node));
	{%- endfor %}
	{%- if not node.mode %}
	write_mode_ref(env, get_irn_mode(node));
	{%- endif %}
	{%- for attr in node.attrs %}
	write_{{attr.type|simplify_type}}(env, get_{{node.name}}_{{attr.name}}(node));
	{%- endfor %}
	{%- if is_dynamic_pinned(node) %}
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

void register_generated_node_readers(void)
{
	{%- for node in nodes|has('serializer') %}
	register_node_reader("{{node.name}}", read_{{node.name}});
	{%- endfor %}
}

void register_generated_node_writers(void)
{
	{%- for node in nodes|has('serializer') %}
	register_node_writer(op_{{node.name}}, write_{{node.name}});
	{%- endfor %}
}
