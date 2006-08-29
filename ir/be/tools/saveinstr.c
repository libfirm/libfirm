/**
 * Helper code to output instrumentation result (see instrument.cpp)
 *  @author Matthias Braun
 */
#include <assert.h>
#include <stdio.h>

extern char __instr;
extern char __instrend;

void __saveinstr()
{
    size_t len;
    FILE* f;

    f = fopen("instr.out", "w");
    assert(f != NULL);

    len = (&__instrend) - (&__instr);
    assert(fwrite(&__instr, len, 1, f) == 1);

    fclose(f);
}
