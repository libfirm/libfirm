#!/usr/bin/python -O
#
# Automatically parse all directories and create the module
# initialization function
#
# author: Michael Beck
#
# $Id$

import sys
import os
import os.path
import getopt
import string
import re

class Processor:
  """This class processes all source files and create the init module."""

  def __init__(self, dirs):
    self.dirs = dirs
    self.ignore_dirs = { "CVS" : "CVS directory" }
    self.allowed_suffixes = [ ".c" ]
    self.reg_string = re.compile(r"BE_REGISTER_MODULE_CONSTRUCTOR\((\w+)\)")
    self._verbose = False
    self.constructors = []
    self.prolog = "\n"
    self.prolog += "void be_init_modules(void)\n"
    self.prolog += "{\n"
    self.prolog += "\tstatic int run_once = 0;\n"
    self.prolog += "\n"
    self.prolog += "\tif (run_once)\n"
    self.prolog += "\t\treturn;\n"
    self.prolog += "\trun_once = 1;\n"
    self.epilog = "}\n\n"

  def verbose(self, flag):
    """enables/disables verbose mode"""
    self._verbose = flag

  def process_dirs(self):
    """process all directories and collect the data"""

    def _visit(self, dirname, names):
      """called for every directory entry"""

      basename = os.path.basename(dirname)
      # check if this directory must be ignored
      if self.ignore_dirs.has_key(basename):
        if self._verbose:
          print "Ignoring",  self.ignore_dirs[basename]
        return

      if self._verbose:
        print "Parsing",  dirname, "..."

      for name in names:
        path = dirname + "/" + name
        if os.path.isfile(path):
          self.process_file(path)

    # code of process_dirs
    for dir in self.dirs:
      os.path.walk(dir, _visit, self)

  def process_file(self, path):
    """process a file"""
    # check, if it has an allowed suffix
    index = path.rfind(".")
    if index < 0:
      return
    suffix = path[index:]
    if suffix in self.allowed_suffixes:
      # we found an allowed suffix
      self.process_src(path)

  def process_src(self, path):
    """process a source file"""
    f = file(path)
    for line in f.readlines():
      m = self.reg_string.search(line)
      if m:
        fkt = m.group(1)
        if self._verbose:
          print "  Found constructor", fkt
        self.constructors.append((fkt, path))
    f.close()

  def update_init_func(self, name):
    """generate the output"""
    self.process_dirs()
    s = ""
    # generate prototypes
    for fkt,path in self.constructors:
      s += "/* %s */\n" % path
      s += "void %s(void);\n" % fkt

    s += self.prolog
    for fkt,path in self.constructors:
      s += "\t%s();\n" % fkt
    s += self.epilog

    if os.access(name, os.F_OK):
      f = file(name)
      olds = f.read()
      f.close()
    else:
      olds = ""

    if olds != s:
      f = file(name, "w")
      f.write(s)
      f.close()
      print name, "updated."
    else:
      print name, "unchanged."

def usage(progname):
  """Prints the usage"""
  print "usage: %s [options] outname directory+ " % progname
  print "Options are:"
  print "-v   verbose"

def main(argv):
  """the main function"""
  dirs = []
  gen_name = None
  verbose = False

  # parse options
  try:
    opts, args = getopt.getopt(argv[1:], "?v", ["help", "verbose"])
  except getopt.GetoptError, msg:
    print "Error:", msg
    usage(argv[0])
    sys.exit(1)

  for opt in opts:
    option, value = opt
    if option in ('-?',  '--help'):
      usage(argv[0])
      sys.exit(0)
    elif option in ('-v', '--verbose'):
      verbose = True

  if len(args) < 2:
    usage(argv[0])
    sys.exit(2)
  else:
    gen_name = args[0]
    dirs = args[1:]

  processor = Processor(dirs)
  processor.verbose(verbose)
  # process the directory and update the requested file
  processor.update_init_func(gen_name)

if __name__ == "__main__":
  main(sys.argv)
