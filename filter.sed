s/#undef  *\([A-Za-z][A-Za-z_]*\)/#undef \1/
s/#define  *\([A-Za-z][A-Za-z_]*\)\(.*\)/#ifndef \1 \
#define \1 \2 \
#endif/
