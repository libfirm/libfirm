/* the loop analysis produces strange loop depths here... */

typedef struct OP {
	int bla;
	int blup;
	int op_seq;
	int op_type;
	struct OP *next;
} OP;

OP operation1;
OP operation2 = { 1, 2, 0, 4, &operation1 };
OP *ptr = &operation2;

int main(int argc, char **argv)
{
	OP *o = ptr;

	if(!o || o->op_seq)
		return;
	printf("%d\n", o->op_seq);
	rand();

	for( ; o; o = o->next) {
		if(o->op_seq)
			break;
		if(o->bla > 52)
			o->bla -= 20;

		switch(o->bla) {
		case 1:
		case 201:
			printf("%d\n", o->bla);
			break;
		default:
			printf("hmm\n");
#if 0
			while(rand() > 10000)
				;
#endif
			break;
		case 500:
			printf("jummy\n");
			break;
		}
	}
	printf("hjo\n");

	return 0;
}
