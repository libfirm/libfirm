{{warning}}
#ifndef FIRM_IR_OPCODES_H
#define FIRM_IR_OPCODES_H

/** The opcodes of the libFirm predefined operations.
 * @ingroup ir_op
 */
typedef enum {{spec.name}}_opcode {
{%- for node in nodes %}
	{{spec.name}}o_{{node.name}},
{%- endfor %}
	{{spec.name}}o_First = {{spec.name}}o_{{nodes[0].name}},
	{{spec.name}}o_Last  = {{spec.name}}o_{{nodes[-1].name}},

{%- if spec.name == "ir" %}
	beo_First,
	/* backend specific nodes */
	beo_Spill = beo_First,
	beo_Reload,
	beo_Perm,
	beo_MemPerm,
	beo_Copy,
	beo_Keep,
	beo_CopyKeep,
	beo_Call,
	beo_Return,
	beo_AddSP,
	beo_SubSP,
	beo_IncSP,
	beo_Start,
	beo_FrameAddr,
	/* last backend node number */
	beo_Last = beo_FrameAddr,
{%- endif %}
	{{spec.name}}o_MaxOpcode
} {{spec.name}}_opcode;

{% for node in nodes %}
/**
 * @ingroup {{node.name}}
 * {{node.name}} opcode
 */
FIRM_API ir_op *op_{{node.name}};
{%- endfor %}

{% for node in nodes %}
/**
 * @ingroup {{node.name}}
 * Returns opcode for {{node.name}} nodes.
 */
FIRM_API ir_op *get_op_{{node.name}}(void);
{%- endfor %}

#endif
