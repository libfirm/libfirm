#ifndef _STAT_DMP_H
#define _STAT_DMP_H

#include "firmstat_t.h"

/**
 * The simple human readable dumper.
 */
extern const dumper_t simple_dumper;

/**
 * the comma separated list dumper
 *
 * @note Limited capabilities, mostly for the Firm paper
 */
extern const dumper_t csv_dumper;

#endif /* _STAT_DMP_H */
