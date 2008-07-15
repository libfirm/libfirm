T tname(k1) (T x) {
	return (x << 5) >> 24;
}

T tname(k2) (T x) {
	return (x >> 5) << 24;
}

T tname(k3) (T x) {
	return (x << 20) >> 20;
}

T tname(k4) (T x) {
	return (x & 0xfff0) >> 8;
}

T tname(k5) (T x) {
	unsigned short l = 24;
	return (x << 5) >> l;
}

T tname(k6) (T x) {
	return (x & 0xff) >> 8;
}

T tname(k7) (T x) {
	return (x >> 10) << 5;
}

T tname(k8) (T x) {
	return (x >> 10) << 5;
}
