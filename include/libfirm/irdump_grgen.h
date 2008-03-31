/*
* Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
*
* This file is part of libFirm.
*
* This file may be distributed and/or modified under the terms of the
* GNU General Public License version 2 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in the
* packaging of this file.
*
* Licensees holding valid libFirm Professional Edition licenses may use
* this file in accordance with the libFirm Commercial License.
* Agreement provided with the Software.
*
* This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
* WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
* PURPOSE.
*/

/**
* @file
* @brief   Write ir graph as a grgen construction rule
* @author  Andreas Schoesser
* @version $Id$
*/


/**
 * Dumps a complete irg in the grgen format
 * irg:			irg to dump
 * filename:    	text file to dump to
 * append:		1 if the new rule should be appended to the file,
 *				otherwise the previous contents are deleted
 **/
void dump_irg_grgen_file(ir_graph *irg, char *filename, int append);

/**
 * Like dump_irg_grgen_file dumps a complete irg in the grgen format
 * irg:			irg to dump
 * suffix:	        suffix for the output file
 *                      (e.g. "main"+ suffix +".grg")
 **/
void dump_irg_grgen(ir_graph *irg, char *suffix);
