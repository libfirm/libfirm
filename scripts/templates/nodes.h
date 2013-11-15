{{warning}}
#ifndef FIRM_GEN_NODES_H
#define FIRM_GEN_NODES_H

{% if spec.external -%}
{% set FIRM_API="" %}
{%- else -%}
{%- set FIRM_API="FIRM_API "-%}
{%- endif -%}

/** The opcodes of the libFirm predefined operations.
 * @ingroup ir_op
 */
typedef enum {{spec.name}}_opcode {
{%- for node in nodes %}
	{{spec.name}}o_{{node.name}},
{%- endfor %}
	{{spec.name}}o_first = {{spec.name}}o_{{nodes[0].name}},
	{{spec.name}}o_last  = {{spec.name}}o_{{nodes[-1].name}},
} {{spec.name}}_opcode;

/**
 * @addtogroup ir_node
 * @{
 */

{% if spec.external %}
int is_{{spec.name}}_node(const ir_node *node);
{{spec.name}}_opcode get_{{spec.name}}_irn_opcode(const ir_node *node);
{% endif %}

{% for node in nodes -%}

/**
 * @defgroup {{node.name}} {{node.name}} node
 *
 * {{node.doc}}
 * @{
 */
{% if node.ins %}
/**
 * Input numbers for {{node.name}} node
 */
typedef enum {
	{%- for input in node.ins %}
	n_{{node.name}}_{{input[0]}},
	{%- endfor %}
	n_{{node.name}}_max = n_{{node.name}}_{{node.ins[-1][0]}}
} n_{{node.name}};
{% endif -%}
{% if node.outs %}
/**
 * Projection numbers for result of {{node.name}} node (use for Proj nodes)
 */
typedef enum {
	{% for out in node.outs -%}
	pn_{{node.name}}_{{out[0]}}
	{%- if out.__len__() > 2 %} = {{out[2]}}{% endif %}, /**< {{out[1]}} */
	{% endfor -%}
	pn_{{node.name}}_max = pn_{{node.name}}_{{node.outs[-1][0]}}
} pn_{{node.name}};
{% endif %}
{%- if not node.noconstructor %}
/**
 * Construct {{node.name|a_an}} node.
 *
 * @param dbgi      A pointer to debug information.
{{ node|blockparameterhelp -}}
{{ node|nodeparametershelp -}}
 */
{{FIRM_API}} ir_node *new_rd_{{node.name}}(
	{%- filter parameters %}
		dbg_info *dbgi
		{{node|blockparameter}}
		{{node|nodeparameters}}
	{% endfilter %});

/**
 * Construct {{node.name|a_an}} node.
 *
{{ node|blockparameterhelp -}}
{{ node|nodeparametershelp -}}
 */
{{FIRM_API}} ir_node *new_r_{{node.name}}(
	{%- filter parameters %}
		{{node|blockparameter}}
		{{node|nodeparameters}}
	{% endfilter %});

/**
 * Construct {{node.name|a_an}} node.
 *
 * @param dbgi      A pointer to debug information.
{{ node|nodeparametershelp -}}
 */
{{FIRM_API}} ir_node *new_d_{{node.name}}(
	{%- filter parameters %}
		dbg_info *dbgi
		{{node|nodeparameters}}
	{% endfilter %});

/**
 * Construct {{node.name|a_an}} node.
 *
{{ node|nodeparametershelp -}}
 */
{{FIRM_API}} ir_node *new_{{node.name}}(
	{%- filter parameters %}
		{{node|nodeparameters}}
	{% endfilter %});
{%- endif %}

/**
 * Test if node is a {{node.name}}
 * @returns 1 if the node is a {{node.name}} node, 0 otherwise
 */
{{FIRM_API}} int is_{{node.name}}(const ir_node *node);

{% for input in node.ins -%}
/** Returns {{input[0]}} input of {{node.name|a_an}} node. */
{{FIRM_API}} ir_node *get_{{node.name}}_{{input[0]}}(const ir_node *node);
/** Sets {{input[0]}} input of {{node.name|a_an}} node. */
{{FIRM_API}} void set_{{node.name}}_{{input[0]}}(ir_node *node, ir_node *{{input[0]|escape_keywords}});
{% endfor -%}
{%- if node.input_name -%}
/** Get the number of {{node.name}} {{node.input_name}}s. */
{{FIRM_API}} int get_{{node.name}}_n_{{node.input_name}}s(ir_node const *node);
/** Get the {{node.name}} {{node.input_name}} with index @p pos. */
{{FIRM_API}} ir_node *get_{{node.name}}_{{node.input_name}}(ir_node const *node, int pos);
/** Set the {{node.name}} {{node.input_name}} with index @p pos. */
{{FIRM_API}} void set_{{node.name}}_{{node.input_name}}(ir_node *node, int pos, ir_node *{{node.input_name}});
/** Get an array of all {{node.name}} {{node.input_name}}s. */
ir_node **get_{{node.name}}_{{node.input_name}}_arr(ir_node *node);
{% endif -%}

{%- for attr in node.attrs|hasnot("noprop") %}
/** Returns {{attr.name}} attribute of {{node.name|a_an}} node. */
{{FIRM_API}} {{attr.type}} get_{{node.name}}_{{attr.name}}(const ir_node *node);
/** Sets {{attr.name}} attribute of {{node.name|a_an}} node. */
{{FIRM_API}} void set_{{node.name}}_{{attr.name}}(ir_node *node, {{attr.type}} {{attr.name}});
{% endfor -%}

/** {{node.name}} opcode */
{{FIRM_API}} ir_op *op_{{node.name}};

/** Returns opcode for {{node.name}} nodes. */
{{FIRM_API}} ir_op *get_op_{{node.name}}(void);

/** @} */

{% endfor -%}

/** @} */

#endif
