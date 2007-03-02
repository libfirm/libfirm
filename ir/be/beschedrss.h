/**
 * Interface for register saturating list scheduler
 * as described in: Sid-Ahmed-Ali Touati
 * Register Saturation in Superscalar and VLIW Codes
 * @author Christian Wuerdig
 * @date   06.09.2006
 * @cvs-id $Id$
 */
#ifndef _BESCHEDRSS_H_
#define _BESCHEDRSS_H_

#include "firm_config.h"

/**
 * Perform RSS schedule preprocessing for the given irg.
 * @param birg  The backend irg object
 */
void rss_schedule_preparation(const be_irg_t *birg);

#endif /* _BESCHEDRSS_H_ */
