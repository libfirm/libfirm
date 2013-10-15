#!/usr/bin/env python
# Update msvc project...
#
# This basically reads in the existing project file and replaces the list of
# files in there while keeping the project settings and compiler flags
# intact...
from lxml import etree as ElementTree
import glob
import os
import uuid

# Namespace handling in ElementTree really sucks...
ms_ns = "http://schemas.microsoft.com/developer/msbuild/2003"
ns = "{%s}" % ms_ns

vcxproj = ElementTree.parse("firm.vcxproj")
project = vcxproj.getroot()

# Find the correct "ItemGroup" element. (somehow there are multiple of them
# in the file). The first one without a "Label" attribute seems to be the
# one we want...
mainItemGroup = None
for igroup in project.findall(ns+"ItemGroup"):
	if "Label" in igroup.attrib:
		continue
	mainItemGroup = igroup
	break
# Remove all existing compile statement, we will recreate them
for comp in (mainItemGroup.findall(ns+"ClCompile") +
		mainItemGroup.findall(ns+"ClInclude") +
		mainItemGroup.findall(ns+"None") +
		mainItemGroup.findall(ns+"CustomBuild")):
	mainItemGroup.remove(comp)

filters = ElementTree.parse("firm.vcxproj.filters")
fproject = filters.getroot()
# The first "ItemGroup" appears to contain directories, the second one
# most of the files. (and the 3rd/4th is unclear to me...)
dirfilters = None
filefilters = None
for igroup in fproject.findall(ns+"ItemGroup"):
	if dirfilters == None:
		dirfilters = igroup
	else:
		filefilters = igroup
		break

# Simply remove everything
for f in dirfilters.findall(ns+"Filter"):
	dirfilters.remove(f)
for f in filefilters.findall(ns+"Filter"):
	filefilters.remove(f)

# simply remove everything

# Directory where we find sourcefiles and at the same time the list of
# directories that will appear in the project.
# Note that you have to specify all parent dirs, so if you have "ir/foo"
# then make sure "ir" is in the list too before "ir/foo"
dirs = [
	"scripts",
	"ir",
	"ir/adt",
	"ir/ana",
	"ir/be",
	"ir/be/scripts",
	"ir/common",
	"ir/debug",
	"ir/ident",
	"ir/ir",
	"ir/libcore",
	"ir/lower",
	"ir/obstack",
	"ir/opt",
	"ir/stat",
	"ir/tr",
	"ir/tv",
	"win32",
	"include",
	"include/libfirm",
	"include/libfirm/adt"
]
backends = [ "ia32", "arm", "amd64", "sparc", "TEMPLATE" ]
dirs += map(lambda x: "ir/be/%s" % x, backends)

# Adds a file to the project. With build type @p type.
# This also adds a filter with the same name as the files directory
def addFile(type, f):
	global filefilters
	inc = "$(FirmRoot)\\%s" % f.replace("/", "\\")
	element = ElementTree.SubElement(mainItemGroup, ns+type, Include=inc)
	fc = ElementTree.SubElement(filefilters, ns+type, Include=inc)
	filt = ElementTree.SubElement(fc, ns+"Filter")
	filt.text = os.path.dirname(f).replace("/", "\\")
	return element

os.chdir("../..")
# "Normal" .c/.h files
for d in dirs:
	# Create the filtergroup for the directory
	wind = d.replace("/", "\\")
	f = ElementTree.SubElement(dirfilters, ns+"Filter", Include=wind)
	ui = ElementTree.SubElement(f, ns+"UniqueIdentifier")
	ui.text = "{%s}" % uuid.uuid4()

	# Compile normal C files
	for cfile in glob.glob("%s/*.c" % d):
		addFile("ClCompile", cfile)
	# "Include" header files
	for hfile in glob.glob("%s/*.h" % d):
		addFile("ClInclude", hfile)

# CustomBuild stuff for the backends
custombuild = []
for be in backends:
	spec = "ir/be/%s/%s_spec.pl" % (be,be)
	custombuild.append(spec)
	cb = addFile("CustomBuild", spec)
	message = ElementTree.SubElement(cb, ns+"Message")
	message.text = "Translate Spec: %(FullPath)"
	command = ElementTree.SubElement(cb, ns+"Command")
	command.text = \
"""$(FirmRoot)\\ir\\be\\scripts\\generate_emitter.pl %%(FullPath) $(FirmRoot)\\ir\\be\\%(arch)s
$(FirmRoot)\\ir\\be\\scripts\\generate_new_opcodes.pl %%(FullPath) $(FirmRoot)\\ir\\be\\%(arch)s
$(FirmRoot)\\ir\\be\\scripts\\generate_regalloc_if.pl %%(FullPath) $(FirmRoot)\\ir\\be\\%(arch)s
$(FirmRoot)\\ir\\be\\scripts\\generate_machine.pl %%(FullPath) $(FirmRoot)\\ir\\be\\%(arch)s
""" % { "arch":be }

	inputfiles = [
			"ir/be/scripts/generate_emitter.pl",
			"ir/be/scripts/generate_new_opcodes.pl",
			"ir/be/scripts/generate_regalloc_if.pl",
			"ir/be/scripts/generate_machine.pl",
	]
	inputfiles = map(lambda x: "$(FirmRoot)\\%s" % x.replace("/", "\\"), inputfiles)
	inputfiles.append("$(AdditonalInputs)")
	inputs = ElementTree.SubElement(cb, ns+"AdditionalInputs")
	inputs.text = ";".join(inputfiles)

	outputfiles = [
			"ir/be/%s/gen_%s_emitter.c" % (be,be),
			"ir/be/%s/gen_%s_emitter.h" % (be,be),
			"ir/be/%s/gen_%s_new_nodes.c" % (be,be),
			"ir/be/%s/gen_%s_new_nodes.h" % (be,be),
			"ir/be/%s/gen_%s_regalloc_if.c" % (be,be),
			"ir/be/%s/gen_%s_regalloc_if.h" % (be,be),
			"ir/be/%s/gen_%s_machine.c" % (be,be),
			"ir/be/%s/gen_%s_machine.h" % (be,be)
	]
	outputfiles = map(lambda x: "$(FirmRoot)\\%s" % x.replace("/", "\\"), outputfiles)
	outputfiles.append("$(Outputs)")
	outputs = ElementTree.SubElement(cb, ns+"Outputs")
	outputs.text = ";".join(outputfiles)

# CustomBuild for ir_io:
custombuild.append("scripts/gen_ir_io.py")
cb = addFile("CustomBuild", "scripts/gen_ir_io.py")
message = ElementTree.SubElement(cb, ns+"Message")
message.text = "Generating I/O code: %(FullPath)"
command = ElementTree.SubElement(cb, ns+"Command")
command.text = "python %(FullPath) $(FirmRoot)\\scripts\\ir_spec.py $(FirmRoot)\\ir\\ir"
additionalinputs = ElementTree.SubElement(cb, ns+"AdditionalInputs")
additionalinputs.text = "$(FirmRoot)\\scripts\\ir_spec.py;%(AdditionalInputs)"
outputs = ElementTree.SubElement(cb, ns+"Outputs")
outputs.text = "$(FirmRoot)\\ir\\ir\\gen_irio_import.inl;$(FirmRoot)\\ir\\ir\\gen_irio_export.inl;$(FirmRoot)\\ir\\ir\\gen_irio_lex.inl;%(Outputs)"

# CustomBuild for ir_spec:
custombuild.append("scripts/gen_ir.py")
cb = addFile("CustomBuild", "scripts/gen_ir.py")
message = ElementTree.SubElement(cb, ns+"Message")
message.text = "Translate IR-Spec: %(FullPath)"
command = ElementTree.SubElement(cb, ns+"Command")
command.text = "python %(FullPath) $(FirmRoot) $(FirmRoot)\\ir\\ir"
additionalinputs = ElementTree.SubElement(cb, ns+"AdditionalInputs")
additionalinputs.text = "$(FirmRoot)\\scripts\\gen_ir.py;%(AdditionalInputs)"
outputs = ElementTree.SubElement(cb, ns+"Outputs")
outputs.text = "$(FirmRoot)\ir\ir\gen_ir_cons.c.inl;$(FirmRoot)\ir\ir\gen_irnode.h;$(FirmRoot)\ir\ir\gen_irnode.c.inl;$(FirmRoot)\ir\ir\gen_irop.c.inl;%(Outputs)"

# Stuff we simply include... but which is neither a .c nor a .h file
stuff = [
	"ir/ir/irflag_t.def",
	"ir/libcore/lc_printf_arg_types.def",
]
stuff += glob.glob("scripts/*.py")
stuff += glob.glob("ir/be/scripts/*.pl")
for s in stuff:
	if s in custombuild:
		continue
	addFile("None", s)

vcxproj.write("/tmp/out.xml")
filters.write("/tmp/outf.xml")

# Use xmllint to reformat the output to something more readable
vcxo = "/tmp/firm.vcxproj"
vcxfo = "/tmp/firm.vcxproj.filters"
os.system("xmllint --format /tmp/out.xml > %s" % vcxo)
os.system("xmllint --format /tmp/outf.xml > %s" % vcxfo)

print("Generated: %s %s" % (vcxo, vcxfo))
