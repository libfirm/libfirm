/*$ -fgvn-pre -fno-gcse $*/

#define MAX_SPEC_FD 3
struct spec_fd_t {
    int limit;
    int len;
    int pos;
    unsigned char *buf;
} spec_fd[MAX_SPEC_FD];

int spec_load (int num, int size) {
    spec_fd[num].len = 0;
    while (spec_fd[num].len < size) {
	int tmp = size - spec_fd[num].len;
	if (tmp > spec_fd[num].len) tmp = spec_fd[num].len;
	spec_fd[num].len += tmp;
    }
    return 0;
}

int main(int argc, char *argv[]) {
	return 0;
}
