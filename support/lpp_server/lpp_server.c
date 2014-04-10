/*
 * This file is part of libFirm.
 * Copyright (C) 2012 Universitaet Karlsruhe
 */

#ifdef _WIN32
#error Sorry, lpp_server is for UNIX only.
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include <netinet/in.h>
#include <netdb.h>

#include <unistd.h>
#include <pthread.h>

#include <time.h>
#include <time.h>
#include <limits.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "debug.h"
#include "list.h"
#include "util.h"

#include "lpp_t.h"
#include "lpp_comm.h"
#include "lpp_solvers.h"

#define MAX_JOBS 128

/* stack size of the solver thread. */
static size_t solver_stack_size = PTHREAD_STACK_MIN;

/* master listening socket. */
static int msock;

/* master semaphore */
static int sem;

/* title of the current process */
static char *title;
static int title_length;

static int n_children = 0;

extern char** environ;

typedef struct _job_t {
	struct list_head list;
	int id;
	pthread_t solver;
	pthread_t session;
	lpp_comm_t *comm;
	lpp_t *lpp;
	lpp_solver_func_t *solver_func;
	time_t received;
	int csock;
} job_t;

#define set_solver_stack_size(size) solver_stack_size = MAX(PTHREAD_STACK_MIN, (size))

#define setproctitle(name, args...) snprintf(title,title_length,(name),##args)

static void initproctitle(int argc, char **argv) {
	int i;
	char **envp = environ;

	/*
	 * Move the environment so we can reuse the memory.
	 * (Code borrowed from sendmail.)
	 * WARNING: ugly assumptions on memory layout here;
	 *          if this ever causes problems, #undef DO_PS_FIDDLING
	 */
	for (i = 0; envp[i] != NULL; i++)
		continue;
	environ = (char **) malloc(sizeof(char *) * (i + 1));
	if (environ == NULL)
		return;
	for (i = 0; envp[i] != NULL; i++)
		if ((environ[i] = strdup(envp[i])) == NULL)
			return;
	environ[i] = NULL;

	title = argv[0];
	if (i > 0)
		title_length = envp[i-1] + strlen(envp[i-1]) - argv[0];
	else
		title_length = argv[argc-1] + strlen(argv[argc-1]) - argv[0];

	argv[1] = NULL;
	memset(title, 0, title_length);
	--title_length;
}

static void job_init(job_t *job, lpp_comm_t *comm, lpp_t *lpp, lpp_solver_func_t *solver_func)
{
  /* TODO MAXJOBS */
  memset(job, 0, sizeof(job[0]));
  job->lpp         = lpp;
  job->solver_func = solver_func;
  job->comm        = comm;
  job->csock       = lpp_comm_fileno(comm);
  job->received    = time(NULL);
  job->session     = pthread_self();
  job->id          = getpid();
}

static firm_dbg_module_t *dbg = NULL;

/**
 * Set up a socket.
 */
static int passive_tcp(uint16_t port, int queue_len)
{
	int s;
	int one = 1;
	struct protoent    *ppe;
	struct sockaddr_in sin;

	memset(&sin, 0, sizeof(sin));
	sin.sin_family       = AF_INET;
	sin.sin_addr.s_addr  = INADDR_ANY;
	sin.sin_port         = htons(port);

	ppe = getprotobyname("tcp");
	s = socket(PF_INET, SOCK_STREAM, ppe->p_proto);
	setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

	if(bind(s, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		perror("bind server socket");
		exit(1);
	}

	if(listen(s, queue_len) < 0) {
		perror("listen server socket");
		exit(1);
	}

	return s;
}

static void *solver_thread(void * data)
{
	job_t *job = data;
	DBG((dbg, LEVEL_1, "starting solver thread pid %d tid %d\n", getpid(), pthread_self()));
	setproctitle("lpp_server [problem solving: %s]", job->lpp->name);

	/* I may be cancelled at every time. */
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	/* solve */
	job->solver_func(job->lpp);

	/* notify the session thread that we are finished. */
	fclose(job->lpp->log);

	return NULL;
}

static int solve(lpp_comm_t *comm, job_t *job)
{
	char buf[1024];
	struct timeval tv;
	fd_set rfds;
	int fds[2];
	FILE *rd, *log;
	int res = 0;
	int retval, fd_rd, fd_comm;
	pthread_attr_t solver_thr_attr;

	struct sembuf semops;
	/* try to acquire a lock for the solver resource. */
	semops.sem_num = 0;
	semops.sem_op = -1;
	semops.sem_flg = SEM_UNDO;
	retval = semop(sem, &semops, 1);
	if(retval < 0) {
		perror("free semaphore");
	}
	DBG((dbg, LEVEL_1, "job %d: solving problem %s\n", job->id, job->lpp->name));

	/* set the log file of the lpp to the pipe. */
	pipe(fds);
	DBG((dbg, LEVEL_4, "pipe read %d write %d\n", fds[0], fds[1]));

	fd_comm = lpp_comm_fileno(comm);
	fd_rd   = fds[0];
	rd      = fdopen(fd_rd, "r");
	log     = fdopen(fds[1], "w");

	lpp_set_log(job->lpp, log);

	/* also set the stack size of the solver thread to a considerable amount */
	pthread_attr_init(&solver_thr_attr);
	pthread_attr_setstacksize(&solver_thr_attr, solver_stack_size);
	pthread_create(&job->solver, &solver_thr_attr, solver_thread, job);

	while(1) {
		/* set select timeout to 10 seconds */
		tv.tv_sec  = 10;
		tv.tv_usec = 0;

		/* set the file descriptors. */
		FD_ZERO(&rfds);
		FD_SET(fd_rd, &rfds);
		FD_SET(fd_comm, &rfds);
		DBG((dbg, LEVEL_4, "select %d %d\n", fd_rd, fd_comm));
		retval = select(MAX(fd_rd, fd_comm) + 1, &rfds, NULL, NULL, &tv);
		DBG((dbg, LEVEL_4, "retval %d\n", retval));

		/* an event on one of the descriptors arrived. */
		if(retval > 0) {
			/* A log message arrived. */
			if(FD_ISSET(fd_rd, &rfds)) {

				/* if there is nothing more to read, the child died; we go home now. */
				if(feof(rd))
					break;

				if(fgets(buf, sizeof(buf), rd)) {
					DBG((dbg, LEVEL_4, "receiving log message %s\n", buf));
					/* send the message over the net. */
					lpp_writel(comm, LPP_CMD_INFO);
					lpp_writes(comm, buf);
					lpp_flush(comm);
				}
			}

			/* A network message arrived. */
			if(FD_ISSET(fd_comm, &rfds)) {
				int cmd;

				retval = read(fd_comm, &cmd, sizeof(cmd));
				if(retval == 0) {
					DBG((dbg, LEVEL_2, "cancelling solver thread tid %d\n", job->solver));
					//pthread_cancel(job->solver);
					exit(1);
					//res = 1;
					//break;
				}

				switch(cmd) {
					/* eat senseless data. */
					default:
						while(read(fd_comm, &cmd, sizeof(cmd)) > 0) {
						}
				}
				res = 1;
			}
		}
	}

	pthread_join(job->solver, NULL);
	semops.sem_num = 0;
	semops.sem_op = 1;
	semops.sem_flg = SEM_UNDO;
	retval = semop(sem, &semops, 1);
	if(retval < 0) {
		perror("free semaphore");
	}

	fclose(rd);
	return res;
}

static void *session(int fd)
{
	lpp_solver_func_t *solver = lpp_find_solver("dummy");
	lpp_comm_t *comm          = lpp_comm_new(fd, LPP_BUFSIZE);

	DBG((dbg, LEVEL_1, "starting session thread pid %d tid %d\n", getpid(), pthread_self()));
	setproctitle("lpp_server [child]");

	/* install the signal handler which gets triggered when the child dies. */
	DBG((dbg, LEVEL_1, "new thread\n"));
	for(;;) {
		char buf[64];
		int cmd = lpp_readl(comm);

		DBG((dbg, LEVEL_2, "command: %s(%d)\n", lpp_get_cmd_name(cmd), cmd));
		setproctitle("lpp_server [command %s(%d)]", lpp_get_cmd_name(cmd), cmd);
		switch(cmd) {
			/* we could not read from the socket, so the connection died. bail out. */
			case -1:
			case LPP_CMD_BAD:
				goto end;

			case LPP_CMD_PROBLEM:
				{
					job_t job;
					lpp_t *lpp;

					lpp = lpp_deserialize(comm);
					lpp_deserialize_values(comm, lpp, lpp_value_start);
					DBG((dbg, LEVEL_3, "problem %s received\n", lpp->name));
					job_init(&job, comm, lpp, solver);

					DBG((dbg, LEVEL_3, "starting job for problem %s\n", lpp->name));
					setproctitle("lpp_server [problem waiting: %s]", lpp->name);

					solve(comm, &job);
					lpp_writel(comm, LPP_CMD_SOLUTION);
					lpp_serialize_stats(comm, lpp);
					lpp_serialize_values(comm, lpp, lpp_value_solution);
					lpp_flush(comm);
					DBG((dbg, LEVEL_1, "job %d: finished with problem %s\n", job.id, lpp->name));
					setproctitle("lpp_server [problem finished: %s]", lpp->name);

					free_lpp(lpp);
				}

				break;

			case LPP_CMD_SOLVER:
				lpp_readbuf(comm, buf, sizeof(buf));
				solver = lpp_find_solver(buf);
				DBG((dbg, LEVEL_2, "setting solver to: %s\n", buf));
				//lpp_send_res(comm, solver != NULL, "could not find solver: %s", buf);
				break;

			case LPP_CMD_SOLVERS: {
				int i;

				for(i = 0; lpp_solvers[i].solver != NULL; i++) {
				}
				lpp_writel(comm, i);

				for(i = 0; lpp_solvers[i].solver != NULL; i++)
					lpp_writes(comm, lpp_solvers[i].name);
				lpp_flush(comm);
				break;
			}

			case LPP_CMD_SET_DEBUG:
				{
					int mask = lpp_readl(comm);
					firm_dbg_set_mask(dbg, mask);
				}
				break;

			case LPP_CMD_BYE:
				goto end;

			default:
				fprintf(stderr, "illegal command %d. exiting\n", cmd);
				goto end;
		}
	}

end:
	/* signal the queue, bail out and we are free now. */
	lpp_comm_free(comm);
	close(fd);

	exit(0);
}

static void child_handler(int sig)
{
	pid_t pid;
	int status;

	(void) sig;

	pid = waitpid(-1, &status, WNOHANG);
	do {
		if(WIFEXITED(status)) {
			DBG((dbg, LEVEL_1, "child %d died normally with return value %d\n", pid, WEXITSTATUS(status)));
			--n_children;
			if(n_children != 0)
				setproctitle("lpp_server [main (%d %s)]", n_children, (n_children>1)?"children":"child");
			else
				setproctitle("lpp_server [main]");
		} else if(WIFSIGNALED(status)) {
			DBG((dbg, LEVEL_1, "child %d died by signal %d\n", pid, WTERMSIG(status)));
			--n_children;
			if(n_children != 0)
				setproctitle("lpp_server [main (%d %s)]", n_children, (n_children>1)?"children":"child");
			else
				setproctitle("lpp_server [main]");
		} else
			DBG((dbg, LEVEL_1, "child %d did something unexpected\n", pid));

		pid = waitpid(-1, &status, WNOHANG);
	} while(pid > 0);
}

static void main_loop(void)
{
	int csock;

	DBG((dbg, LEVEL_1, "master pid %d\n", getpid()));

	msock = passive_tcp(LPP_PORT, 10);

	for(;;) {
		struct sockaddr_in fsin;
		socklen_t len = sizeof(fsin);
		pid_t child;

		csock = accept(msock, (struct sockaddr *) &fsin, &len);
		if(csock < 0) {
			if(errno == EINTR)
				continue;
			else
				fprintf(stderr, "could not accept: %s\n", strerror(errno));
		}

		child = fork();
		switch(child) {
			case 0: /* we're in the new child, start the session handler */
				close(msock);
				session(csock);
				break;
			case -1: /* error, die! */
				perror("fork");
				exit(1);
			default: /* if we're in the parent just continue */
				++n_children;
				setproctitle("lpp_server [main (%d %s)]", n_children, (n_children>1)?"children":"child");
				close(csock);
				break;
		}
	}
}

static void toggle_dbg(int num)
{
	static int mask = 0;
	(void) num;
	mask = ~mask;
	firm_dbg_set_mask(dbg, mask);
}

#define SEMKEYPATH "/tmp/lppkey"
#define SEMKEYID 42

static void print_syntax(void) {
	fprintf(stderr, "lpp_server [-g <dbg level>] [-s <thread stack size>]\n");
}

int main(int argc, char *argv[])
{
	key_t semkey;
	int ret;
	int c;
	struct sigaction sigact1, sigact2;

	dbg = firm_dbg_register("lpp.server");
	firm_dbg_set_mask(dbg, 1);

	set_solver_stack_size(128 * 1024 * 1024);

	/* parse options. */
	while((c = getopt(argc, argv, "s:g:")) != -1) {
		switch(c) {
			case 's':
				set_solver_stack_size(atoi(optarg));
				break;
			case 'g':
				firm_dbg_set_mask(dbg, atoi(optarg));
				break;
			default:
				print_syntax();
				exit(1);
		}
	}

	initproctitle(argc, argv);
	setproctitle("lpp_server [main]");

	memset(&sigact1, 0, sizeof(sigact1));
	sigact1.sa_handler = toggle_dbg;
	sigaction(SIGUSR1, &sigact1, NULL);

	memset(&sigact2, 0, sizeof(sigact2));
	sigact2.sa_handler = child_handler;
	sigact2.sa_flags = SA_NOCLDSTOP;
	sigaction(SIGCHLD, &sigact2, NULL);

	/* set up semaphore */
	semkey = ftok(SEMKEYPATH,SEMKEYID);
	if ( semkey == (key_t)-1 ) {
		perror("ftok() for sem failed");
		return 1;
	}

	sem = semget( semkey, 1, 0666 | IPC_CREAT);// | IPC_EXCL );
	if ( sem == -1 ) {
		perror("semget() failed");
		return -1;
	}

	ret = semctl(sem, 0, SETVAL, 1);
	if ( ret < 0 ) {
		perror("semctl() failed");
		return -1;
	}

	main_loop();

	ret = semctl( sem, 0, IPC_RMID );
	if (ret < 0) {
		printf("semctl() remove id failed\n");
		return -1;
	}
	return 0;
}
