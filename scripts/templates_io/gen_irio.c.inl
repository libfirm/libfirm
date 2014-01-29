{{warning}}
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
	register_node_reader("{{node.name}}", read_{{node.name}});
	{%- endfor %}
}

static void register_generated_node_writers(void)
{
	{%- for node in nodes|notset('customSerializer') %}
	register_node_writer(op_{{node.name}}, write_{{node.name}});
	{%- endfor %}
}
