#ifndef __CES_TIME_MEASURE_H__
#define __CES_TIME_MEASURE_H__
 
struct ces_time {
	unsigned long cpu;
/*
	unsigned long wall;
	unsigned long cycles;
	int perfsimple_fd_cycles;
*/
};

void ces_time_start(struct ces_time* ces_time);
void ces_time_stop(struct ces_time* ces_time);
void ces_time_print(const char * const title, struct ces_time* time);
#endif // __CES_TIME_MEASURE_H__
