set unwindonsignal on

# FIRM

# The following is able to print most important firm datastructures:
# ir_node*, tarval*, ir_type*, ir_mode* and maybe others should work 
define irn
print gdb_node_helper($arg0)
end

# Hack to display the length of a firm ARR_F or ARR_D
define arrlen
p array_len($arg0)
end

# The following should be used for libfirm after 1.18.0
define dumpg
if $argc == 1
	call dump_ir_graph($arg0, "XXX")
else
	call dump_ir_graph(current_ir_graph, "XXX")
end
end

define firmd
call firm_debug($arg0)
end

define graph
print gdb_node_helper(current_ir_graph)
end

define keep
call add_End_keepalive(get_irg_end(current_ir_graph), $arg0)
end

# cparser
define cpexpr
call print_expression($arg0), (void)putchar('\n')
end

define cpstmt
call print_statement($arg0)
end

define cptype
call print_type($arg0), (void)putchar('\n')
end

define bs
call bitset_fprint_dots(stderr,env->outputs)
call puts("")
end
