#ifndef _IA32_NEW_NODES_H_
#define _IA32_NEW_NODES_H_

/**
 * Function prototypes for the assembler ir node constructors.
 * @author Christian Wuerdig
 * $Id$
 */

#include "ia32_nodes_attr.h"

/**
 * Returns the name of a firm symconst.
 */
const char *get_sc_name(ir_node *symc);



/***************************************************************************************************
 *        _   _                   _       __        _                    _   _               _
 *       | | | |                 | |     / /       | |                  | | | |             | |
 *   __ _| |_| |_ _ __   ___  ___| |_   / /_ _  ___| |_   _ __ ___   ___| |_| |__   ___   __| |___
 *  / _` | __| __| '__| / __|/ _ \ __| / / _` |/ _ \ __| | '_ ` _ \ / _ \ __| '_ \ / _ \ / _` / __|
 * | (_| | |_| |_| |    \__ \  __/ |_ / / (_| |  __/ |_  | | | | | |  __/ |_| | | | (_) | (_| \__ \
 *  \__,_|\__|\__|_|    |___/\___|\__/_/ \__, |\___|\__| |_| |_| |_|\___|\__|_| |_|\___/ \__,_|___/
 *                                        __/ |
 *                                       |___/
 ***************************************************************************************************/

/**
 * Returns the attributes of an ia32 node.
 */
ia32_attr_t *get_ia32_attr(const ir_node *node);

/**
 * Gets the type of an ia32 node.
 */
ia32_op_type_t get_ia32_op_type(const ir_node *node);

/**
 * Sets the type of an ia32 node.
 */
void set_ia32_op_type(ir_node *node, ia32_op_type_t tp);

/**
 * Gets the supported addrmode of an ia32 node
 */
ia32_am_type_t get_ia32_am_support(const ir_node *node);

/**
 * Sets the supported addrmode of an ia32 node
 */
void set_ia32_am_support(ir_node *node, ia32_am_type_t am_tp);

/**
 * Gets the addrmode flavour of an ia32 node
 */
ia32_am_flavour_t get_ia32_am_flavour(const ir_node *node);

/**
 * Sets the addrmode flavour of an ia32 node
 */
void set_ia32_am_flavour(ir_node *node, ia32_am_flavour_t am_flavour);

/**
 * Gets the joined addrmode offset.
 */
char *get_ia32_am_offs(const ir_node *node);

/**
 * Adds an offset for addrmode.
 */
void add_ia32_am_offs(ir_node *node, char *offset);

/**
 * Subs an offset for addrmode.
 */
void sub_ia32_am_offs(ir_node *node, char *offset);

/**
 * Gets the addr mode const.
 */
int get_ia32_am_scale(const ir_node *node);

/**
 * Sets the const for addr mode.
 */
void set_ia32_am_scale(ir_node *node, int scale);

/**
 * Return the tarval of an immediate operation or NULL in case of SymConst
 */
tarval *get_ia32_Immop_tarval(const ir_node *node);

/**
 * Sets the attributes of an immediate operation to the specified tarval
 */
void set_ia32_Immop_tarval(ir_node *node, tarval *tv);

/**
 * Return the sc attribute.
 */
char *get_ia32_sc(const ir_node *node);

/**
 * Sets the sc attribute.
 */
void set_ia32_sc(ir_node *node, char *sc);

/**
 * Gets the string representation of the internal const (tv or symconst)
 */
char *get_ia32_cnst(const ir_node *node);

/**
 * Sets the uses_frame attribute.
 */
void set_ia32_use_frame(ir_node *node, char flag);

/**
 * Gets the uses_frame attribute
 */
char get_ia32_use_frame(const ir_node *node);

/**
 * Gets the mode of the stored/loaded value (only set for Store/Load)
 */
ir_mode *get_ia32_ls_mode(const ir_node *node);

/**
 * Sets the mode of the stored/loaded value (only set for Store/Load)
 */
void set_ia32_ls_mode(ir_node *node, ir_mode *mode);

/**
 * Returns the argument register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_in_req_all(const ir_node *node);

/**
 * Returns the result register requirements of an ia32 node.
 */
const ia32_register_req_t **get_ia32_out_req_all(const ir_node *node);

/**
 * Returns the argument register requirements of an ia32 node.
 */
const ia32_register_req_t *get_ia32_in_req(const ir_node *node, int pos);

/**
 * Returns the result register requirements of an ia32 node.
 */
const ia32_register_req_t *get_ia32_out_req(const ir_node *node, int pos);

/**
 * Sets the OUT register requirements at position pos.
 */
void set_ia32_req_out(ir_node *node, const ia32_register_req_t *req, int pos);

/**
 * Sets the IN register requirements at position pos.
 */
void set_ia32_req_in(ir_node *node, const ia32_register_req_t *req, int pos);

/**
 * Returns the register flag of an ia32 node.
 */
arch_irn_flags_t get_ia32_flags(const ir_node *node);

/**
 * Sets the register flag of an ia32 node.
 */
void set_ia32_flags(const ir_node *node, arch_irn_flags_t flags);

/**
 * Returns the result register slots of an ia32 node.
 */
const arch_register_t **get_ia32_slots(const ir_node *node);

/**
 * Returns the name of the OUT register at position pos.
 */
const char *get_ia32_out_reg_name(const ir_node *node, int pos);

/**
 * Returns the index of the OUT register at position pos within its register class.
 */
int get_ia32_out_regnr(const ir_node *node, int pos);

/**
 * Returns the OUT register at position pos.
 */
const arch_register_t *get_ia32_out_reg(const ir_node *node, int pos);

/**
 * Sets the number of results.
 */
void set_ia32_n_res(ir_node *node, int n_res);

/**
 * Returns the number of results.
 */
int get_ia32_n_res(const ir_node *node);

/**
 * Returns the flavour of an ia32 node,
 */
ia32_op_flavour_t get_ia32_flavour(const ir_node *node);

/**
 * Sets the flavour of an ia32 node to flavour_Div/Mod/DivMod/Mul/Mulh.
 */
void set_ia32_flavour(ir_node *node, ia32_op_flavour_t op_flav);

/**
 * Returns the projnum code.
 */
long get_ia32_pncode(const ir_node *node);

/**
 * Sets the projnum code
 */
void set_ia32_pncode(ir_node *node, long code);



/******************************************************************************************************
 *                      _       _         _   _           __                  _   _
 *                     (_)     | |       | | | |         / _|                | | (_)
 *  ___ _ __   ___  ___ _  __ _| |   __ _| |_| |_ _ __  | |_ _   _ _ __   ___| |_ _  ___  _ __    ___
 * / __| '_ \ / _ \/ __| |/ _` | |  / _` | __| __| '__| |  _| | | | '_ \ / __| __| |/ _ \| '_ \  / __|
 * \__ \ |_) |  __/ (__| | (_| | | | (_| | |_| |_| |    | | | |_| | | | | (__| |_| | (_) | | | | \__ \
 * |___/ .__/ \___|\___|_|\__,_|_|  \__,_|\__|\__|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_| |___/
 *     | |
 *     |_|
 ******************************************************************************************************/

/**
 * Gets the type of an ia32_Const.
 */
unsigned get_ia32_Const_type(const ir_node *node);

/**
 * Sets the type of an ia32_Const.
 */
void set_ia32_Const_type(ir_node *node, int type);

/**
 * Copy the attributes from an ia32_Const to an Immop (Add_i, Sub_i, ...) node
 */
void set_ia32_Immop_attr(ir_node *node, ir_node *cnst);

/**
 * Copy the attributes from a Const to an ia32_Const
 */
void set_ia32_Const_attr(ir_node *ia32_cnst, ir_node *cnst);

/**
 * Sets the AddrMode attribute
 * @param direction The "direction" of AM ('S' source or 'D' destination)
 */
void set_ia32_AddrMode(ir_node *node, char direction);

/**
 * Returns whether or not the node is an AddrModeS node.
 */
int is_ia32_AddrModeS(const ir_node *node);

/**
 * Returns whether or not the node is an AddrModeD node.
 */
int is_ia32_AddrModeD(const ir_node *node);

/**
 * Checks if node is a Load or fLoad.
 */
int is_ia32_Ld(const ir_node *node);

/**
 * Checks if node is a Store or fStore.
 */
int is_ia32_St(const ir_node *node);

/* Include the generated headers */
#include "gen_ia32_new_nodes.h"

#endif /* _IA32_NEW_NODES_H_ */
