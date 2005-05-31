/**
 * @file   offset.h
 * @date   31.05.2005
 * @author Sebastian Hack
 *
 * Firm's own offset_of and container_of
 *
 * Copyright (C) 2005 Universitaet Karlsruhe
 * Released under the GPL
 */

#ifndef _OFFSET_H
#define _OFFSET_H

/**
 * Get the offset of a member of a struct.
 * @param type   The type of the struct member is in.
 * @param member The name of the member.
 * @return       The offset of member in type in bytes.
 */
#define offset_of(type, member) \
  ((char *) &(((type *) 0)->member) - (char *) 0)

/**
 * Make pointer to the struct from a pointer to a member of that struct.
 * @param ptr     The pointer to the member.
 * @param type    The type of the struct.
 * @param member  The name of the member.
 * @return        A pointer to the struct member is in.
 */
#define container_of(ptr, type, member) \
	((type *) ((char *) (ptr) - offset_of(type, member)))

#endif /* _OFFSET_H */
