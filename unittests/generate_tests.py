#!/usr/bin/env python3

from os import path
from glob import glob
import re, subprocess, os

def create_irp(source):
    subprocess.run(["cparser", "--export-ir", source], check=True)
    return path.splitext(source)[0] + ".ir"

expected_re = re.compile(r"//\s*FIRM_IMPORTANT_ARGS\s*:\s*(\d+)")
def parse_expected(source):
    with open(source, 'r') as f:
        contents = f.read()
    result = expected_re.search(contents)
    return int(result.group(1)) if result else 1

test_template = """
#include "lib/important_args__local.h"

int main(int argc, char *argv[])
{
    (void) argc;
	test_irg_has_expected_important_args("%s", %u, argv[0]);
	return 0;
}
"""

tests_dir = path.dirname(path.realpath(__file__))
fixtures_dir = path.join(tests_dir, "fixtures")

# Cleanup previously generated files
test_files = glob(path.join(tests_dir, "_*.c"))
ir_files = glob(path.join(fixtures_dir, "**/*.ir"), recursive=True)
for f in test_files + ir_files:
    os.remove(f)

# Generate tests
sources = glob(path.join(fixtures_dir, "**/*.c"), recursive=True)
for source in sources:
    source_dir, source_file = path.split(source)
    module_name = path.basename(source_dir)

    print("Generating IR representation of %s" % source_file)
    os.chdir(source_dir)
    ir_path = create_irp(source)
    expected_set = parse_expected(source)

    test_content = test_template % (ir_path, expected_set)
    file_name = "_%s__%s" % (module_name, source_file)
    with open(path.join(tests_dir, file_name), "w") as f:
        f.write(test_content)
