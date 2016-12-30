# This file is part of libFirm.
# Copyright (C) 2015 Matthias Braun
from jinja2 import BaseLoader


class SimpleLoader(BaseLoader):
    """
    Simple FileSystemLoader variant. Compared to the default loader in jinja
    it does not perform searchpath magic and does not reject paths containig
    ``..`` (the jinja one does that for security reasons).
    Note that simply using env.from_string is a not a good alternative as
    then we miss filename and linenumber in error messages from jinja.
    """
    def __init__(self):
        super(SimpleLoader, self).__init__()
        self.includedirs = [""]

    def get_source(self, environment, name):
        for dir in self.includedirs:
            try:
                path = name if dir == "" else "%s/%s" % (dir, name)
                contents = open(path).read()
            except:
                continue

            def uptodate():
                return False

            return contents, name, uptodate
        raise Exception("Could not open '%s'" % name)

    def list_template(self):
        return []


exports = dict()


def export(thing, name=None):
    if name is None:
        name = thing.__name__
    assert name not in exports
    exports[name] = thing


filters = dict()


def export_filter(func, name=None):
    if name is None:
        name = func.__name__
    assert name not in filters
    filters[name] = func
