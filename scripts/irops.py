from jinjautil import export_filter, export
from jinja2._compat import string_types
from filters import arguments
import imp
import sys


class Node(object):
    '''Node base class'''
    only_regular = False
    pinned = "no"
    flags = []
    ins = []
    attrs = []
    constructor = True
    constructor_args = []
    block = None
    mode = None
    pinned_init = None
    throws_init = None
    arity_override = None


abstracts = set()


def abstract(cls):
    abstracts.add(cls)
    return cls


def is_abstract(nodetype):
    return nodetype in abstracts


def op(cls):
    cls.__is_firm_op = True
    # Without new-style classes it is hard to detect the inheritance hierarchy
    # later.
    assert hasattr(cls, "__class__"), "must use new-style classes"
    return cls


def is_op(nodetype):
    return hasattr(nodetype, "__is_firm_op")


class Attribute(object):
    def __init__(self, name, type, comment="", init=None, to_flags=None,
                 noprop=False, fqname=None):
        self.type = type
        self.name = name
        self.comment = comment
        self.init = init
        self.to_flags = to_flags
        self.noprop = noprop
        if fqname is None:
            fqname = name
        self.fqname = fqname


class Operand(object):
    pass


def Input(name, comment=None):
    op = Operand()
    op.name = name
    op.comment = comment
    return op


def Output(name, comment=None):
    op = Operand()
    op.name = name
    op.comment = comment
    return op


def verify_node(node):
    if node.pinned not in ["yes", "no", "exception"]:
        print("%s: UNKNOWN PINNED MODE: %s" % (node.name, node.pinned))

    if node.pinned_init is not None and not is_dynamic_pinned(node):
        print("ERROR: node %s has pinned_init attribute but is not marked as dynamically pinned" % node.name)
    if "uses_memory" in node.flags:
        if not inout_contains(node.ins, "mem"):
            print("ERROR: memory op %s needs an input named 'mem'" %
                  node.name)
    if "fragile" in node.flags:
        if not is_dynamic_pinned(node):
            print("ERROR: fragile node %s must be dynamically pinned" %
                  node.name)
        if node.throws_init is None:
            print("ERROR: fragile node %s needs a throws_init attribute" %
                  node.name)
        if not inout_contains(node.outs, "X_regular"):
            print("ERROR: fragile node %s needs an output named 'X_regular'" %
                  node.name)
        if not inout_contains(node.outs, "X_except") and not node.only_regular:
            print("ERROR: fragile node %s needs an output named 'X_except'" %
                  node.name)
    else:
        if node.throws_init is not None:
            print("ERROR: throws_init only makes sense for fragile nodes")


def setldefault(node, attr, val):
    # Don't use hasattr, as these things should not be inherited
    if attr not in node.__dict__:
        setattr(node, attr, val)


def setdefault(node, attr, val):
    if not hasattr(node, attr):
        setattr(node, attr, val)


def setnodedefaults(node):
    setldefault(node, "name", node.__name__)

    # As a shortcut you can specify inputs either as a list of strings or
    # as a list of (name, comment) tuples. Normalize it to Input objects
    new_ins = []
    for i in node.ins:
        if isinstance(i, string_types):
            i = Input(i)
        elif isinstance(i, tuple):
            i = Input(name=i[0], comment=i[1])
        new_ins.append(i)
    node.ins = new_ins
    if hasattr(node, "outs"):
        new_outs = []
        for o in node.outs:
            if isinstance(o, string_types):
                o = Output(o)
            elif isinstance(o, tuple):
                o = Output(name=o[0], comment=o[1])
            new_outs.append(o)
        node.outs = new_outs

    if hasattr(node, "__doc__"):
        node.doc = trim_docstring(node.__doc__)
    else:
        node.doc = ""

    if not is_abstract(node):
        setdefault(node, "arity", len(node.ins))
        setdefault(node, "attrs_name", node.name.lower())
        setdefault(node, "serializer", node.constructor)
        if hasattr(node, "outs") and len(node.outs) > 1:
            node.mode = "mode_T"
        if "start_block" in node.flags:
            node.block = "get_irg_start_block(irg)"
        setdefault(node, "usesGraph", node.block is not None)


def trim_docstring(docstring):
    if not docstring:
        return ''
    # Convert tabs to spaces (following the normal Python rules)
    # and split into a list of lines:
    lines = docstring.expandtabs().splitlines()
    # Determine minimum indentation (first line doesn't count):
    indent = sys.maxsize
    for line in lines[1:]:
        stripped = line.lstrip()
        if stripped:
            indent = min(indent, len(line) - len(stripped))
    # Remove indentation (first line is special):
    trimmed = [lines[0].strip()]
    if indent < sys.maxsize:
        for line in lines[1:]:
            trimmed.append(line[indent:].rstrip())
    # Strip off trailing and leading blank lines:
    while trimmed and not trimmed[-1]:
        trimmed.pop()
    while trimmed and not trimmed[0]:
        trimmed.pop(0)
    # Return a single string:
    return '\n'.join(trimmed)


def parameterlist(parameterlist):
    return "\n".join(parameterlist)


def nodearguments(node):
    arguments = [arg.name for arg in node.arguments]
    return parameterlist(arguments)


def nodeparameters(node):
    parameters = ["%s %s" % (arg.type, arg.name) for arg in node.arguments]
    return parameterlist(parameters)


def nodeparametershelp(node):
    res = ""
    for param in node.arguments:
        res += " * @param %-9s %s\n" % (param.name, param.comment)
    return res


def a_an(text):
    if text[0] in "aAeEuUoOiI":
        return "an " + text
    return "a " + text


def blockparameter(node):
    if not node.block:
        return "ir_node *block"
    elif node.usesGraph:
        return "ir_graph *irg"
    else:
        return ""


def blockparameterhelp(node):
    if not node.block:
        return " * @param block     The IR block the node belongs to.\n"
    elif node.usesGraph:
        return " * @param irg       The IR graph the node belongs to.\n"
    else:
        return ""


def blockargument(node):
    if not node.block:
        return "block"
    elif node.usesGraph:
        return "irg"
    else:
        return ""


def blockassign(node):
    if node.block:
        return "ir_node *block = %s;" % node.block
    else:
        return ""


def irgassign(node):
    if node.usesGraph:
        return ""
    else:
        return "ir_graph *irg = get_irn_irg(block);\n"


def curblock(node):
    if not node.block:
        return "get_cur_block()"
    elif node.usesGraph:
        return "current_ir_graph"
    else:
        return ""


def insdecl(node):
    arity = node.arity
    if arity == "dynamic" or arity == "variable":
        if len(node.ins) == 0:
            return ""
        insarity = len(node.ins)
        res = "int r_arity = arity + " + repr(insarity) + ";"
        res += "\n\tir_node **r_in= ALLOCAN(ir_node*, r_arity);"
        i = 0
        for input in node.ins:
            res += "\n\tr_in[" + repr(i) + "] = irn_" + input.name + ";"
            i += 1
        res += "\n\tMEMCPY(&r_in[" + repr(insarity) + "], in, arity);\n\t"
    elif arity == 0:
        return ""
    else:
        res = "ir_node *in[" + repr(arity) + "];"
        i = 0
        for input in node.ins:
            res += "\n\tin[" + repr(i) + "] = irn_" + input.name + ";"
            i += 1
    return res


def arity_and_ins(node):
    arity = node.arity
    if arity == "dynamic" or arity == "variable":
        if len(node.ins) == 0:
            return "arity, in"
        else:
            return "r_arity, r_in"
    elif arity == 0:
        return "0, NULL"
    else:
        return repr(arity) + ", in"


def arity(node):
    if node.arity_override is not None:
        return node.arity_override
    arity = node.arity
    if arity == "dynamic":
        return "oparity_dynamic"
    return "oparity_any"


def pinned(node):
    pinned = node.pinned
    if pinned == "yes":
        return "op_pin_state_pinned"
    if pinned == "no":
        return "op_pin_state_floats"
    if pinned == "exception":
        return "op_pin_state_exc_pinned"
    print("WARNING: Unknown pinned state %s in format pined" % pinned)
    return ""


def flags(node):
    flags = list(map(lambda x: "irop_flag_" + x, node.flags))
    if not flags:
        flags = ["irop_flag_none"]
    return " | ".join(flags)


def stringformat(string, *args):
    return string % args


def attr_size(node):
    if not hasattr(node, "attr_struct"):
        return "0"
    return "sizeof(%s)" % node.attr_struct


def opindex(node):
    if hasattr(node, "op_index"):
        return node.op_index
    return "-1"


keywords = frozenset(["true", "false"])


def escape_keywords(word):
    if word in keywords:
        return word + "_"
    return word


def parameters(string):
    return arguments(string, voidwhenempty=True)


def args(arglist):
    argument_names = [arg.name for arg in arglist]
    return "\n".join(argument_names)


def block(node):
    if not node.block:
        return "block"
    elif node.usesGraph:
        return "env->irg"
    else:
        return ""


def simplify_type(string):
    """Returns a simplified version of a C type for use in a function name.
    Stars are replaced with _ref, spaces removed and the ir_ firm namespace
    prefix stripped."""
    res = string.replace("*", "_ref").replace(" ", "")
    if res.startswith("ir_"):
        res = res[3:]
    return res


for f in [a_an, args, arity_and_ins, arity, attr_size, blockargument, block,
          blockparameter, blockparameterhelp, curblock, escape_keywords,
          flags, insdecl, blockassign, irgassign, nodearguments,
          nodeparameters, nodeparametershelp, opindex, parameterlist,
          parameters, pinned, simplify_type, stringformat]:
    export_filter(f)


def _preprocess_node(node):
    # construct node arguments
    arguments = []
    initattrs = []
    for input in node.ins:
        arguments.append(
            Attribute("irn_" + input.name, type="ir_node *",
                      comment=input.name))

    if node.arity == "variable" or node.arity == "dynamic":
        arguments.append(
            Attribute("arity", type="int",
                      comment="size of additional inputs array"))
        arguments.append(
            Attribute("in", type="ir_node *const *",
                      comment="additional inputs"))

    if node.mode is None:
        arguments.append(
            Attribute("mode", type="ir_mode *",
                      comment="mode of the operations result"))

    for attr in node.attrs:
        if attr.init is not None:
            continue
        arguments.append(attr)

    # dynamic pin state means more constructor arguments
    if is_dynamic_pinned(node):
        if node.pinned_init is not None:
            initattrs.append(
                Attribute("pinned", fqname="exc.pinned",
                          type="int", init=node.pinned_init))
        else:
            arguments.append(
                Attribute("pinned", type="int", comment="pinned state"))
            initattrs.append(
                Attribute("pin_state", fqname="exc.pinned",
                          type="int", init="pinned"))
    if node.throws_init is not None:
        initattrs.append(
            Attribute("throws_exception", fqname="exc.throws_exception",
                      type="unsigned", init=node.throws_init))

    for arg in node.constructor_args:
        arguments.append(arg)

    node.arguments = arguments
    node.initattrs = initattrs


def prepare_nodes(namespace):
    nodes = []
    for x in namespace.values():
        if not is_op(x):
            continue
        setnodedefaults(x)
        verify_node(x)
        nodes.append(x)
    nodes.sort(key=lambda x: x.name)
    if len(nodes) == 0:
        print("Warning: No nodes found in spec file '%s'" % filename)

    real_nodes = []
    abstract_nodes = []
    for node in nodes:
        if is_abstract(node):
            abstract_nodes.append(node)
        else:
            real_nodes.append(node)
            _preprocess_node(node)

    return (real_nodes, abstract_nodes)


def is_dynamic_pinned(node):
    return node.pinned == "exception"


def is_fragile(node):
    return "fragile" in node.flags


def inout_contains(l, name):
    for entry in l:
        if entry.name == name:
            return True
    return False


def collect_ops(moduledict):
    return [node for node in moduledict.values() if is_op(node)]


def verify_spec(spec):
    if len(spec.nodes) == 0:
        sys.stderr.write("Warning: No nodes found in spec\n")
    if not hasattr(spec, "name"):
        sys.stderr.write("Warning: No name specified in node spec\n")


export(is_dynamic_pinned)
export(is_abstract)
