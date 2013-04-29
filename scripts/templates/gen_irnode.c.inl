{{warning}}
{%- for node in nodes %}

int (is_{{node.name}})(const ir_node *node)
{
	return is_{{node.name}}_(node);
}
{%  for attr in node.attrs|hasnot("noprop") %}
{{attr.type}} (get_{{node.name}}_{{attr.name}})(const ir_node *node)
{
	return get_{{node.name}}_{{attr.name}}_(node);
}

void (set_{{node.name}}_{{attr.name}})(ir_node *node, {{attr.type}} {{attr.name}})
{
	set_{{node.name}}_{{attr.name}}_(node, {{attr.name}});
}
{% endfor -%}
{%- for input in node.ins %}
ir_node *(get_{{node.name}}_{{input[0]}})(const ir_node *node)
{
	return get_{{node.name}}_{{input[0]}}(node);
}

void (set_{{node.name}}_{{input[0]}})(ir_node *node, ir_node *{{input[0]|escape_keywords}})
{
	set_{{node.name}}_{{input[0]}}_(node, {{input[0]|escape_keywords}});
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
{%- endif -%}
{%- endfor -%}
