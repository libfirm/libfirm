#!/usr/bin/env python3

from os import path
from glob import glob
import re, subprocess, os

def create_irp(source):
    subprocess.run(["cparser", "--export-ir", source], check=True)
    return path.splitext(source)[0] + ".ir"

test_re = re.compile(r"//\s*(assert_.+)$", re.M)
def parse_tests(source):
    with open(source, 'r') as f:
        contents = f.read()
    result = test_re.findall(contents)
    return "\n\t".join(result)

test_template = """
#include "lib/%s_test_lib.h"

int main(void)
{
	setup("%s");

	%s

	teardown();
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
    tests = parse_tests(source)

    test_content = test_template % (module_name, ir_path, tests)
    file_name = "_%s__%s" % (module_name, source_file)
    with open(path.join(tests_dir, file_name), "w") as f:
        f.write(test_content)
