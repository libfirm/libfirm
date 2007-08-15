#! /usr/bin/env python

import sys
import fileinput
import os
import re

from optparse import OptionParser

def ev_create_stmt(heads):
	create = 'create table if not exists ev (id int, time int, timeev int'
	for x in heads:
		create += (', %s real' % (x,))
	create += ');'
	return create

def ctx_create_stmt(heads):
	create = 'create table if not exists ctx (id int unique on conflict ignore'
	for x in heads:
		create += (', %s text' % (x,))
	create += ');'
	return create

def find_heads(file):
	ctx_heads = set()
	ev_heads = set()
	for line in fileinput.input(file):
		items = re.split('\s+', line)
		if items[0] == 'E':
			ev_heads.add(items[2])
		elif items[0] == 'P':
			ctx_heads.add(items[2])
	return (ev_heads, ctx_heads)

def fill_tables(file):
	keystack = []
	valstack = []
	for line in fileinput.input(file):
		items = re.split('\s+', line)
		op    = items[0]
		id    = items[1]
		if op == 'P':
			key   = items[2]
			val   = items[3]
			keystack.append(key)
			valstack.append(val)
			stmt = 'insert into ctx (id'
			for x in keystack:
				stmt += ', ' + x
			stmt += ') values (' + str(int(id, 16))
			for x in valstack:
				stmt += ', \'' + x + '\''
			stmt += ');'
		elif op == 'O':
			keystack.pop()
			valstack.pop()
			stmt = ''
		elif op == 'E':
			key   = items[2]
			val   = items[3]
			time   = items[4]
			timeev = items[5]
			t      = (id, val, time, timeev)
			stmt   = 'insert into ev (id, %s, time, timeev) values (%d, %s, %s, %s);' % (key, int(id, 16), val, time, timeev)

		if stmt:
			print stmt


def main():
	parser = OptionParser('usage: %prog [options] input')
	parser.add_option("-s", "--schema", action="store_true", help="emit only the schema definitions")
	parser.add_option("-e", "--events", action="store_true", help="emit only the event information")
	(options, args) = parser.parse_args()

	if len(args) < 1:
		parser.print_help()
		sys.exit(1)

	file = args[0]

	if not os.path.isfile(file):
		print "cannot find input file %s" % (file, )
		sys.exit(2)

	(ev_heads, ctx_heads) = find_heads(file)

	if not options.schema and not options.events:
		options.schema = 1
		options.events = 1

	if options.schema:
		print ev_create_stmt(ev_heads)
		print ctx_create_stmt(ctx_heads)

	if options.events:
		print 'begin transaction;'
		fill_tables(file)
		print 'commit;'


if __name__ == "__main__":
	main()
