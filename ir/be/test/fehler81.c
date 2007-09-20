int main(int argc, char *aregv[]) {
	switch (argc) {
	case 1:
		goto a;
	default:
		break;
	a:
		printf("Here\n");
	}
	return 0;
}
