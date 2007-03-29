/**
 * Simple naive instrumentation on (x86) assembler files
 * @author Matthias Braun (warning this is a 30 minutes hack and no nice code!)
 *
 * Takes an assembler file and adds instrumentation code to some instructions.
 * You have to link the output with saveinstr.c
 *
 * Example usage with a file queens.s:
 *
 *    (adapt is_instr function here to your needs and compile instrument.cpp)
 *    instrument < queens.s > queensi.s
 *    gcc queensi.s saveinstr.c -o queensi
 *    ./queensi     	(run with you desired arguments and testdata)
 *    instrument -a < queens.s > queens_annotated.s
 *    You will get a list of the hottest spots in your file and an assembler
 *    will the instructions prefixed with their execution count
 */
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <vector>
#include <string>
#include <algorithm>

/** checks if we have an instruction that should be instrumented */
bool is_instr(const char* str)
{
    if(str[0] != '\t')
        return false;
    if(str[1] == '.')
        return false;
    if(isspace(str[1]))
        return false;
    if(str[1] == 'c' && str[2] == 'a' && str[3] == 'l')
        return true;
    if(str[1] == 'j')
        return true;
    if(str[1] == 'p')
        return true;

    return false;
}

struct place {
    int line;
    unsigned int count;

	bool operator <(const place& other) const
	{
		return count > other.count;
	}
};

int main(int argc, char** argv)
{
    char linebuf[2048];
    int instrcount = 0;
	bool annotate = false;
    std::vector<place> places;
	int line = 0;

    FILE* file = stdin;
	FILE* instr = NULL;

	// annotation mode?
	if(argc == 2 && std::string(argv[1]) == "-a") {
		annotate = true;
		instr = fopen("instr.out", "r");
		assert(instr != NULL);
	}

    while(!feof(file)) {
        fgets(linebuf, sizeof(linebuf), file);
		line++;
        if(feof(file))
            break;

        if(is_instr(linebuf)) {
			if(annotate) {
				place p;
				p.line = line;
				assert(fread(&p.count, 4, 1, instr) == 1);
				printf("/* %u */ ", p.count);

				places.push_back(p);
			} else {
				printf("\tpushf\n");
				printf("\tinc DWORD PTR[__instr + %d]\n", (instrcount++) * 4);
				printf("\tpopf\n");
			}
        }

        printf("%s", linebuf);

		if(!annotate) {
			if(strcmp(linebuf, "main:\n") == 0) {
				printf("\tsub %%esp, 4\n"
						"\tmov DWORD PTR[%%esp], OFFSET FLAT:__saveinstr\n"
						"\tcall atexit\n"
						"\tadd %%esp, 4\n");
			}
		}
    }

	if(!annotate) {
		printf("\n\t.section .bss\n"
				".global __instr\n"
				"__instr:\n"
				"\t.space %d\n"
				".global __instrend\n"
				"__instrend:\n", instrcount * 4);
	} else {
		std::sort(places.begin(), places.end());

		for(int i = 0; i < 20 && i < places.size(); ++i) {
			const place &p = places[i];
			fprintf(stderr, "line %d: %u\n", p.line, p.count);
		}
	}

    fclose(file);
}
