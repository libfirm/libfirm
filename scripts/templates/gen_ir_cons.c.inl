{{warning}}
{%- for node in nodes %}
{%- if not node.noconstructor %}
ir_node *new_rd_{{node.name}}(
	{%- filter parameters %}
		dbg_info *dbgi
		{{node|blockparameter}}
		{{node|nodeparameters}}
	{% endfilter %})
{
	{{node|irgassign}}
	{{node|insdecl}}

	ir_node *res = new_ir_node(
		{%- filter arguments %}
			dbgi
			irg
			{{node.block}}
			op_{{node.name}}
			{{node.mode}}
			{{node|arity_and_ins}}
		{% endfilter %});
	{%- if node.arity == "dynamic" %}
	for (int i = 0; i < arity; ++i) {
		add_irn_n(res, in[i]);
	}
	{%- endif %}
	{%- for attr in node.attrs %}
	res->attr.{{node.attrs_name}}{{attr["fqname"]}} =
		{%- if "init" in attr %} {{ attr["init"] -}};
		{%- else              %} {{ attr["name"] -}};
		{%- endif %}
	{%- endfor %}
	{%- for attr in node.initattrs %}
	res->attr.{{node.attrs_name}}{{attr["fqname"]}} = {{ attr["init"] -}};
	{%- endfor %}
	{{- node.init }}
	irn_verify_irg(res, irg);
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
{%- endfor %}
