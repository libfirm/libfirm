#! /usr/bin/env python

import sys
import os
import re
import time
import stat
import profile
import sqlite3
import MySQLdb
import fileinput
import tempfile
import optparse

class DummyFilter:
	def match(self, dummy):
		return True

class EmitBase:
	def create_table(self, cols, name, type, unique):
		create = 'create table if not exists %s (id int %s' % (name, unique)

		sorted = [None] * len(cols)
		for x in cols.iterkeys():
			sorted[cols[x]] = x
		for x in sorted:
			create += (', %s %s' % (x, type))
		create += ');'
		return create

class EmitMysqlInfile(EmitBase):
	tmpfile_mode = stat.S_IREAD | stat.S_IROTH | stat.S_IWUSR

	def ex(self, args, tab, fname):
		res = os.fork()
		if res == 0:
			stmt = """load data infile '%s' into table %s fields terminated by ';'""" % (fname, tab)
			conn = MySQLdb.connect(**args)
			c = conn.cursor()
			c.execute(stmt)
			conn.commit()
			sys.exit(0)
		return res

	def __init__(self, options, ctxcols, evcols):
		args = dict()
		if options.password:
			args['passwd'] = options.password
		if not options.host:
			options.host = 'localhost'
		args['user'] = options.user
		args['host'] = options.host
		args['db']   = options.database

		self.conn     = MySQLdb.connect(**args)
		self.ctxcols  = ctxcols
		self.evcols   = evcols
		self.options  = options

		params = (tempfile.gettempdir(), os.sep, os.getpid())
		self.evfifo  = '%s%sstatev_ev_%d' % params
		self.ctxfifo = '%s%sstatev_ctx_%d' % params

		os.mkfifo(self.evfifo)
		os.mkfifo(self.ctxfifo)

		os.chmod(self.evfifo,  self.tmpfile_mode)
		os.chmod(self.ctxfifo, self.tmpfile_mode)

		c = self.conn.cursor()
		c.execute('drop table if exists ev')
		c.execute('drop table if exists ctx')
		c.execute(self.create_table(self.ctxcols, 'ctx', 'char(80)', 'unique'))
		c.execute(self.create_table(self.evcols, 'ev', 'double default null', ''))
		self.conn.commit()

		if options.verbose:
			print 'go for gold'

		self.pidev  = self.ex(args, 'ev', self.evfifo)
		self.pidctx = self.ex(args, 'ctx', self.ctxfifo)

		if options.verbose:
			print "forked two mysql leechers: %d, %d" % (self.pidev, self.pidctx)

		self.evfile   = open(self.evfifo, 'w+t')
		self.ctxfile  = open(self.ctxfifo, 'w+t')

		if options.verbose:
			print 'fifo:  %s, %o' % (self.evfile.name, os.stat(self.evfile.name).st_mode)
			print 'fifo:  %s, %o' % (self.ctxfile.name, os.stat(self.ctxfile.name).st_mode)

	def ev(self, curr_id, evitems):
		field = ['\N'] * len(self.evcols)
		for key, val in evitems.iteritems():
			index = self.evcols[key]
			field[index] = val
		print >> self.evfile, ('%d;' % curr_id) + ';'.join(field)

	def ctx(self, curr_id, ctxitems):
		field = ['\N'] * len(self.ctxcols)
		for key, val in ctxitems.iteritems():
			index = self.ctxcols[key]
			field[index] = val
		print >> self.ctxfile, ('%d;' % curr_id) + ';'.join(field)

	def commit(self):
		self.evfile.close()
		self.ctxfile.close()

		os.waitpid(self.pidev, 0)
		os.waitpid(self.pidctx, 0)

		os.unlink(self.evfile.name)
		os.unlink(self.ctxfile.name)


class EmitSqlite3(EmitBase):
	def __init__(self, options, ctxcols, evcols):
		if os.path.isfile(options.database):
			os.unlink(options.database)

		self.conn = sqlite3.connect(options.database)
		self.conn.execute(self.create_table(ctxcols, 'ctx', 'text', 'unique'))
		self.conn.execute(self.create_table(evcols, 'ev', 'double', ''))

		n = max(len(ctxcols), len(evcols)) + 1
		q = ['?']
		self.quests = []
		for i in xrange(0, n):
			self.quests.append(','.join(q))
			q.append('?')

	def ev(self, curr_id, evitems):
		keys = ','.join(evitems.keys())
		stmt = 'insert into ev (id, %s) values (%s)' % (keys, self.quests[len(evitems)])
		self.conn.execute(stmt, (curr_id,) + tuple(evitems.values()))

	def ctx(self, curr_id, ctxitems):
		keys = ','.join(ctxitems.keys())
		stmt = 'insert into ctx (id, %s) values (%s)' % (keys, self.quests[len(ctxitems)])
		self.conn.execute(stmt, (curr_id,) + tuple(ctxitems.values()))

	def commit(self):
		self.conn.commit()

class Conv:
	engines = { 'sqlite3': EmitSqlite3, 'mysql': EmitMysqlInfile }
	def find_heads(self):
		n_ev    = 0
		ctxind   = 0
		evind    = 0
		ctxcols  = dict()
		evcols   = dict()

		self.valid_keys = set()

		for line in self.input():
			heads = None
			if line[0] == 'P':
				ind = line.index(';', 2)
				key = line[2:ind]
				if not key in ctxcols:
					ctxcols[key] = ctxind
					ctxind += 1

			elif line[0] == 'E':
				ind = line.index(';', 2)
				key = line[2:ind]
				if self.filter.match(key):
					self.n_events += 1
					if not key in evcols:
						self.valid_keys.add(key)
						evcols[key] = evind
						evind += 1

		return (ctxcols, evcols)

	def input(self):
		return fileinput.FileInput(files=self.files, openhook=fileinput.hook_compressed)

	def fill_tables(self):
		lineno     = 0
		ids        = 0
		curr_id    = 0
		keystack   = []
		idstack    = []
		curr_event = 0
		last_prec  = -1
		evcols     = dict()
		ctxcols    = dict()

		for line in self.input():
			lineno += 1
			items = line.strip().split(';')
			op    = items[0]

			if op == 'P':
				# flush the current events
				if len(evcols):
					self.emit.ev(curr_id, evcols)
					evcols.clear()

				# push the key
				key   = items[1]
				val   = items[2]
				keystack.append(key)
				curr_id = ids
				ids += 1
				idstack.append(curr_id)
				ctxcols[key] = val

				self.emit.ctx(curr_id, ctxcols)

			elif op == 'O':
				popkey = items[1]
				key = keystack.pop()

				if popkey != key:
					print "unmatched pop in line %d, push key %s, pop key: %s" % (lineno, key, popkey)

				idstack.pop()
				if len(idstack) > 0:
					if len(evcols) > 0:
						self.emit.ev(curr_id, evcols)
						evcols.clear()
					del ctxcols[key]
					curr_id = idstack[-1]
				else:
					curr_id = -1

			elif op == 'E':
				key = items[1]
				if key in self.valid_keys:
					curr_event += 1
					evcols[key] = items[2]

					if self.verbose:
						prec = curr_event * 10 / self.n_events
						if prec > last_prec:
							last_prec = prec
							print '%10d / %10d' % (curr_event, self.n_events)

	def __init__(self):
		parser = optparse.OptionParser('usage: %prog [options]  <event file...>')
		parser.add_option("-c", "--clean",    dest="clean",    help="delete tables in advance", action="store_true", default=False)
		parser.add_option("-v", "--verbose",  dest="verbose",  help="verbose messages",         action="store_true", default=False)
		parser.add_option("-f", "--filter",   dest="filter",   help="regexp to filter event keys", metavar="REGEXP")
		parser.add_option("-u", "--user",     dest="user",     help="user",               metavar="USER")
		parser.add_option("-H", "--host",     dest="host",     help="host",               metavar="HOST")
		parser.add_option("-p", "--password", dest="password", help="password",           metavar="PASSWORD")
		parser.add_option("-d", "--db",       dest="database", help="database",           metavar="DB")
		parser.add_option("-e", "--engine",   dest="engine",   help="eingine",            metavar="ENG", default='sqlite3')
		(options, args) = parser.parse_args()

		self.n_events = 0
		self.stmts    = dict()
		self.verbose  = options.verbose

		if len(args) < 1:
			parser.print_help()
			sys.exit(1)

		self.files  = []
		files       = args

		for file in files:
			if not os.path.isfile(file):
				print "cannot find input file %s" % (file, )
			else:
				self.files.append(file)

		if len(self.files) < 1:
			print "no input file to process"
			sys.exit(3)

		if options.filter:
			self.filter = re.compile(options.filter)
		else:
			self.filter = DummyFilter()

		if options.engine in self.engines:
			engine = self.engines[options.engine]
		else:
			print 'engine %s not found' % options.engine
			print 'we offer: %s' % self.engines.keys()
			sys.exit(0)

		if options.verbose:
			print "determining schema..."

		(ctxcols, evcols) = self.find_heads()
		if options.verbose:
			print "context schema:"
			print ctxcols
			print "event schema:"
			print evcols

		self.emit = engine(options, ctxcols, evcols)

		if options.verbose:
			print "filling tables..."
		self.fill_tables()
		if options.verbose:
			print "comitting..."
		self.emit.commit()

if __name__ == "__main__":
	Conv()
