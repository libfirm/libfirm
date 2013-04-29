{{warning}}
{% for node in nodes %}
ir_op *op_{{node.name}}; ir_op *get_op_{{node.name}}(void) { return op_{{node.name}}; }
{%- endfor %}

static void generated_init_op(void)
{
	{%- for node in nodes %}
	op_{{node.name}} = new_ir_op(
		{%- filter arguments %}
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
	ir_op_set_fragile_indices(op_{{node.name}}, pn_{{node.name}}_X_regular, pn_{{node.name}}_X_except);
	{%- endif -%}
	{%- endfor %}
}

static void generated_finish_op(void)
{
	{%- for node in nodes %}
	free_ir_op(op_{{node.name}}); op_{{node.name}} = NULL;
	{%- endfor %}
}
