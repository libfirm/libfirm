/*
 * This file is part of libFirm.
 * Copyright (C) 2012 University of Karlsruhe.
 */

/**
 * @file
 * @brief   Input/Output textual representation of firm.
 * @author  Moritz Kroll
 */
#ifndef FIRM_IR_IRIO_H
#define FIRM_IR_IRIO_H

#include <stdio.h>

#include "firm_types.h"
#include "begin.h"

/**
 * @defgroup irio Input and Output
 * @note The file format is not considered stable yet. So expect
 *       incompatibilities between file formats of different libfirm versions.
 * @{
 */

/**
 * Exports the whole irp to the given file in a textual form.
 * Exports all types, all ir graphs, and the constant graph.
 *
 * @param filename  the name of the resulting file
 * @return  0 if no errors occured, other values in case of errors
 */
FIRM_API int ir_export(const char *filename);

/**
 * same as ir_export but writes to a FILE*
 * @note As with any FILE* errors are indicated by ferror(output)
 */
FIRM_API void ir_export_file(FILE *output);

/**
 * Imports the data stored in the given file.
 * Imports any type graphs and ir graphs contained in the file.
 *
 * @param filename  the name of the file
 * @returns 0 if no errors occured, other values in case of errors
 */
FIRM_API int ir_import(const char *filename);

/**
 * same as ir_import but imports from a FILE*
 */
FIRM_API int ir_import_file(FILE *input, const char *inputname);

/**
 * If the linker option 'shared' is specified we pass it to the backend.
 */
FIRM_API void lto_set_shared_flag(void);

/**
 * Passes the amount of ir-files to load to the backend.
 */
FIRM_API void lto_set_ir_file_count(int count);

/**
 * Some adjustments are only allowed if we do not export to an ir-file.
 */
FIRM_API void lto_set_mode_is_compile_executable(int isExport);


/** @} */

#include "end.h"

#endif
