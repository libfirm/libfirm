#!/usr/bin/env python3

from os import path
from glob import glob
import sys, re, subprocess, os

test_re = re.compile(r"//\s*(assert_.+)$", re.M)
def parse_test_stmnts(source):
    with open(source, 'r') as f:
        contents = f.read()
    result = test_re.findall(contents)
    return "\n\t".join(result)

test_template = """
#include "testlib.h"

int main(int argc, const char **argv)
{
	(void)argc;
	setup(argv[1]);

	%s

	teardown();
	return 0;
}
"""

# Generate tests
source_path = sys.argv[1]
target_path = sys.argv[2]

test_stmnts = parse_test_stmnts(source_path)
test_content = test_template % test_stmnts

with open(target_path, "w") as f:
    f.write(test_content)
