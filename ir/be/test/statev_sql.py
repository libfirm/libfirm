#! /usr/bin/env python

import sys
import fileinput
import os
import re
import sha
import sqlite3

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

def fill_tables(conn, file):
	curr_id  = 0
	ids      = dict()
	valstack = []
	keystack = []
	for line in fileinput.input(file):
		items = re.split('\s+', line)
		op    = items[0]
		id    = items[1]

		if op == 'P':
			key   = items[2]
			val   = items[3]
			keystack.append(key)
			valstack.append(val)

			dig = sha.new()
			for i in xrange(0,len(keystack)):
				dig.update(".")
				dig.update(keystack[i])
				dig.update("=")
				dig.update(valstack[i])

			hash = dig.digest()
			if hash in ids:
				id = ids[hash]
			else:
				id        = curr_id
				ids[hash] = curr_id
				curr_id   = curr_id + 1

			stmt = 'insert into ctx (id'
			for x in keystack:
				stmt += ', ' + x
			stmt += ') values (' + str(id)
			for x in valstack:
				stmt += ', \'' + x + '\''
			stmt += ');'
			conn.execute(stmt)

		elif op == 'O':
			keystack.pop()
			valstack.pop()

		elif op == 'E':
			key   = items[2]
			val   = items[3]
			time   = items[4]
			timeev = items[5]
			t      = (id, val, time, timeev)
			stmt   = 'insert into ev (id, %s, time, timeev) values (?, ?, ?, ?)' % (key,)
			conn.execute(stmt, (id, val, time, timeev))


def main():
	parser = OptionParser('usage: %prog [options] <sqlite3  database> <event file>')
	parser.add_option("-c", "--clean",  action="store_true", help="clean existing data base before adding data")
	(options, args) = parser.parse_args()

	if len(args) < 2:
		parser.print_help()
		sys.exit(1)

	db   = args[0]
	file = args[1]

	if not os.path.isfile(file):
		print "cannot find input file %s" % (file, )
		sys.exit(2)

	have_to_clean = 0
	if os.path.isfile(db):
		if options.clean:
			have_to_clean = 1
		else:
			print "database %s already exists (use different name or use -c)" % (db, )
			sys.exit(3)

	(ev_heads, ctx_heads) = find_heads(file)

	conn = sqlite3.connect(db)
	if have_to_clean:
		conn.execute("drop table ctx")
		conn.execute("drop table ev")

	conn.execute(ev_create_stmt(ev_heads))
	conn.execute(ctx_create_stmt(ctx_heads))
	fill_tables(conn, file)
	conn.commit()

if __name__ == "__main__":
	main()
