BEGIN           { output = 0
                 print "#ifndef FIRM_CONFIG_FIRM_CONFIG_H"
                 print "#define FIRM_CONFIG_FIRM_CONFIG_H"
                 print "/* This file was automtically generated from libFirm's configure */"
                }
/snap,[ ]*snap/ { output = 0; NEXT }
                { if (output) print }
/snip,[ ]*snip/ { output = 1 }
END             { print "\n#endif\n" }
