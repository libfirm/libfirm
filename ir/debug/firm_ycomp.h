#ifndef _FIRM_YCOMP_H_
#define _FIRM_YCOMP_H_

#include "firmnet.h"

/**
 * Establish connection to yComp and register debugger hooks.
 * @param host Hostname where yComp is running
 * @param port Port on which yComp is listening
 */
void firm_init_ycomp_debugger(const char *host, uint16_t port);

/**
 * Close connection to yComp and unregister debugger hooks.
 */
void firm_finish_ycomp_debugger(void);

#endif /* _FIRM_YCOMP_H_ */
