/*
 * Copyrigth (C) 1995-2007 University of Karlsruhe.  All right reserved.
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
 * @brief  Implementation of offset_of and container_of
 * @date   31.05.2005
 * @author Sebastian Hack
 */
#ifndef FIRM_ADT_OFFSET_H
#define FIRM_ADT_OFFSET_H

/**
 * Get the offset of a member of a struct.
 * @param type   The type of the struct member is in.
 * @param member The name of the member.
 * @return       The offset of member in type in bytes.
 */
#define firm_offset_of(type, member)		((char *) &((type *) 0)->member - (char *) 0)

/**
 * Make pointer to the struct from a pointer to a member of that struct.
 * @param ptr     The pointer to the member.
 * @param type    The type of the struct.
 * @param member  The name of the member.
 * @return        A pointer to the struct member is in.
 */
#define firm_container_of(ptr, type, member)		((type *) ((char *) (ptr) - firm_offset_of(type, member)))

#endif
