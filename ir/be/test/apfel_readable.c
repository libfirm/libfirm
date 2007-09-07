#include <stdio.h>

float o=0.075,h=1.5,T,r,O,l,I;
int _,L=80,s=3200;

int main(int argc, char *argv[]){
	for (; s;) {
		if (s%L == 0) {
			h -= o;
			T = -2;
		}

		O = I*2*O+h;
		I = l+T-r;
		if (4 -(r=O*O)<(l=I*I) | ++_==L) {
			int index = --s%L ? (_ < L ? --_ %6:6) : 7;
			char c = "Sascha \n"[index];
			putchar(c);
			O = I = l = _ = r = 0;
			T += o/2;
		}
	}
	return 0;
}
