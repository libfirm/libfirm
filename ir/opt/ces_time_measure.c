#include "ces_time_measure.h"
#include "ces_si_tools.h"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#define _GNU_SOURCE
#include <unistd.h>
#include <stdlib.h>
#include <linux/perf_event.h>
#include <time.h>

/*
 * tried 
 * - gettimeofday: gives wall clock
 * - clock: 1µs resolution in linux, a lot cheaper than perfcount
 * - perfcount: 1µs resolution but somehow more expensive, maybe due to multiple ioctl calls
 * decided on clock, removed rest
 */

/* forward declarations */
inline static unsigned long get_cpu_time(void);
inline static unsigned long get_wall_time(void);
 

/* implementation */
void ces_time_start(struct ces_time* time) {
	time->cpu = get_cpu_time();

}

void ces_time_stop(struct ces_time* time) {
	unsigned long cpu_end;
	cpu_end = get_cpu_time();
	time->cpu = cpu_end - time->cpu;
}

void ces_time_print(const char * const title, struct ces_time* time) {
	DB((ces_dbg, LEVEL_DEFAULT, "timer resolution %uµs\n",CLOCKS_PER_SEC/1000000));
	DB((ces_dbg, LEVEL_3, "time for %s: cpu:%fs\n",title, (float)time->cpu/1000000));
}

/*   private implementation  */
inline static unsigned long get_wall_time(){
		struct timeval time;
		if (gettimeofday(&time,NULL)){
				return 0;
		}
		return (unsigned long) (time.tv_usec + 1000000 * time.tv_sec);
}

inline static unsigned long get_cpu_time(){
	return clock();
}

/*
static int _perfsimple_measure_start(struct ces_time *time);
static void _perfsimple_measure_end(struct ces_time *time);
static long perf_event_open( struct perf_event_attr *hw_event, pid_t pid, int cpu, int group_fd, unsigned long flags );
#define perfsimple_measure_start(x) _perfsimple_measure_start(x), ioctl(x->perfsimple_fd_cycles, PERF_EVENT_IOC_ENABLE, 0)
#define perfsimple_measure_end(x) ioctl(x->perfsimple_fd_cycles, PERF_EVENT_IOC_DISABLE, 0), _perfsimple_measure_end(x)

static long perf_event_open( struct perf_event_attr *hw_event, pid_t pid, int cpu, int group_fd, unsigned long flags ) {
	return syscall( __NR_perf_event_open, hw_event, pid, cpu, group_fd, flags );
}

static int _perfsimple_measure_start(struct ces_time* time) {
	struct perf_event_attr pe_cycles;

	memset(&pe_cycles, 0, sizeof(struct perf_event_attr));

	pe_cycles.type = PERF_TYPE_HARDWARE;
	pe_cycles.size = sizeof(struct perf_event_attr);
	pe_cycles.config = PERF_COUNT_HW_CPU_CYCLES;
	pe_cycles.disabled = 1;
	pe_cycles.exclude_kernel = 0;
	pe_cycles.exclude_hv = 1;

	time->perfsimple_fd_cycles = perf_event_open(&pe_cycles, 0, -1, -1, 0);
	if (time->perfsimple_fd_cycles < 0)  return -1;

	ioctl(time->perfsimple_fd_cycles, PERF_EVENT_IOC_RESET, 0);
	// ioctl(perfsimple_fd_cycles, PERF_EVENT_IOC_ENABLE, 0); 

	return 0;
}

static void _perfsimple_measure_end(struct ces_time *time) {
  // ioctl(perfsimple_fd_cycles, PERF_EVENT_IOC_DISABLE, 0);
	
	read(time->perfsimple_fd_cycles, &(time->cycles), sizeof(long long));
	close(time->perfsimple_fd_cycles);
	time->cycles = time->cycles / (3.1*1000); //usec
}
*/
