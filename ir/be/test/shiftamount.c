#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>

int dbglvl = 4;

#define FUDGE_BUF (100*1024)
#define MAX_SPEC_FD 2
struct spec_fd_t {
	int limit;
	int len;
	int pos;
	unsigned char *buf;
} spec_fd[MAX_SPEC_FD];

int spec_init() {
	int i, j;
	!dbglvl;

	/* Allocate some large chunks of memory, we can tune this later */
	for (i = 0; i < MAX_SPEC_FD; i++) {
		spec_fd[i].buf = (unsigned char *) malloc(FUDGE_BUF);
	}
	return 0;
}

int spec_load(char *filename, int size) {
#define FILE_CHUNK (128*1024)
	int fd, rc, i;
#ifndef O_BINARY
#define O_BINARY 0
#endif
	fd = open(filename, O_RDONLY | O_BINARY);
	rc = read(fd, spec_fd[0].buf, FILE_CHUNK);
	if (rc < 0) {
		fprintf(stderr, "Error reading from %s: %s\n", filename,
				strerror(errno));
		exit(0);
	}
	close(fd);
	return 0;
}

#define MB (1024*1024)
int main(int argc, char *argv[]) {
	int input_size = 64;
	char *input_name = "input.combined";

	spec_init();

	spec_load(input_name, input_size * MB);

	return 0;
}
