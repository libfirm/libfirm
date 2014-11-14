{{warning}}
{% for node in nodes %}
{%- if node.outs %}
static const pns_lookup_t {{node.name}}_lut[] = {
	{%- for out in node.outs %}
	{ pn_{{node.name}}_{{out.name}}, "{{out.name}}" },
	{%- endfor %}
};
{% endif -%}
{%- endfor %}

static const proj_lookup_t proj_lut[] = {
	{%- for node in nodes -%}
	{%- if node.outs %}
	{ {{spec.name}}o_{{node.name}}, ARRAY_SIZE({{node.name}}_lut), {{node.name}}_lut },
	{%- endif %}
	{%- endfor %}
};
