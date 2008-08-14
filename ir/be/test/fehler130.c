/*$ -fcombo $*/
static int bi_valid;
static int bi_buf = 0;

int outbuf;

#define Buf_size (8 * 2*sizeof(char))

#define put_byte(c) {outbuf=(unsigned char)(c); }

#define put_short(w) \
{ if (outcnt > 0) { \
    outbuf = (unsigned char) (w); \
  }\
}

void send_bits(int value) {
    if (bi_valid > 0) {
        bi_buf |= bi_valid;
        if (outbuf > 0) {
    		outbuf = bi_buf;
  		}
        bi_buf = value;
    }
}

int main() {
	return 0;
}
