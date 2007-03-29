struct foo {
  int bar;
};

struct foo *curcmd;
int tmps_max;
int tmps_base;

#define CF_FLIP 020000
#define CF_COND 01000

int main() {
    struct foo *cmd;
    int cmdflags;

    if (cmdflags & CF_COND) {

    goto maybe;
    flipmaybe:
	if (cmdflags) {
	    while (tmps_max > tmps_base) {	/* clean up after last eval */
	    }
	}
    maybe:
      curcmd = cmd;
    }

    return 0;
}
