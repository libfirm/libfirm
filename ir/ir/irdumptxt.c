/*
 * Project:     libFIRM
 * File name:   ir/ir/irdump.c
 * Purpose:     Write vcg representation of firm to file.
 * Author:      Martin Trapp, Christian Schaefer
 * Modified by: Goetz Lindenmaier, Hubert Schmidt
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1998-2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "irdump.h"

#include "firm_common_t.h"

#include "irgraph_t.h"
#include "irprog_t.h"
#include "entity_t.h"

int dump_node_opcode(FILE *F, ir_node *n); /* from irdump.c */



#define X(a)    case a: fprintf(F, #a); break
void    dump_entity_to_file_prefix (FILE *F, entity *ent, char *prefix, unsigned verbosity) {
  int i, j;
  assert(ent && ent->kind == k_entity);
  type *owner = get_entity_owner(ent);
  type *type  = get_entity_type(ent);

  if (verbosity & dump_verbosity_onlynames) {
    fprintf(F, "%sentity %s (%ld)\n", prefix, get_entity_name(ent), get_entity_nr(ent));
    return;
  }

  if (verbosity & dump_verbosity_entattrs) {
    fprintf(F, "%sentity %s (%ld)\n", prefix, get_entity_name(ent), get_entity_nr(ent));
    fprintf(F, "%s  type:  %s (%ld)\n", prefix, get_type_name(type),  get_type_nr(type));
    fprintf(F, "%s  owner: %s (%ld)\n", prefix, get_type_name(owner), get_type_nr(owner));

    if (is_class_type(get_entity_owner(ent))) {
      if (get_entity_n_overwrites(ent) > 0) {
	fprintf(F, "%s  overwrites:\n", prefix);
	for (i = 0; i < get_entity_n_overwrites(ent); ++i) {
	  entity *ov = get_entity_overwrites(ent, i);
	  fprintf(F, "%s    %d: %s of class %s\n", prefix, i, get_entity_name(ov),
		  get_type_name(get_entity_owner(ov)));
	}
      } else {
	fprintf(F, "%s  Does not overwrite other entities. \n", prefix);
      }
      if (get_entity_n_overwrittenby(ent) > 0) {
	fprintf(F, "%s  overwritten by:\n", prefix);
	for (i = 0; i < get_entity_n_overwrittenby(ent); ++i) {
	  entity *ov = get_entity_overwrittenby(ent, i);
	  fprintf(F, "%s    %d: %s of class %s\n", prefix, i, get_entity_name(ov),
		  get_type_name(get_entity_owner(ov)));
	}
      } else {
	fprintf(F, "%s  Is not overwriten by other entities. \n", prefix);
      }
    }

    fprintf(F, "%s  allocation:  ", prefix);
    switch (get_entity_allocation(ent)) {
      X(allocation_dynamic);
      X(allocation_automatic);
      X(allocation_static);
      X(allocation_parameter);
    }

    fprintf(F, "\n%s  visibility:  ", prefix);
    switch (get_entity_visibility(ent)) {
      X(visibility_local);
      X(visibility_external_visible);
      X(visibility_external_allocated);
    }

    fprintf(F, "\n%s  variability: ", prefix);
    switch (get_entity_variability(ent)) {
      X(variability_uninitialized);
      X(variability_initialized);
      X(variability_part_constant);
      X(variability_constant);
    }
    fprintf(F, "\n");
  } else {  /* no entityattrs */
    fprintf(F, "%s(%3d) %*s: %s", prefix,
	    get_entity_offset_bits(ent), -40, get_type_name(get_entity_type(ent)), get_entity_name(ent));
    if (is_method_type(get_entity_type(ent))) fprintf(F, "(...)");
    fprintf(F, "\n");
  }

  if (verbosity & dump_verbosity_entconsts) {
    if (get_entity_variability(ent) != variability_uninitialized) {
      if (is_atomic_entity(ent)) {
	fprintf(F, "%s  atomic value: ", prefix);
	dump_node_opcode(F, get_atomic_ent_value(ent));
      } else {
	fprintf(F, "%s  compound values:", prefix);
	for (i = 0; i < get_compound_ent_n_values(ent); ++i) {
	  compound_graph_path *path = get_compound_ent_value_path(ent, i);
	  entity *ent0 = get_compound_graph_path_node(path, 0);
	  fprintf(F, "\n%s    %3d ", prefix, get_entity_offset_bits(ent0));
	  if (get_type_state(type) == layout_fixed)
	    fprintf(F, "(%3d) ",   get_compound_ent_value_offset_bits(ent, i));
	  fprintf(F, "%s", get_entity_name(ent0));
	  for (j = 0; j < get_compound_graph_path_length(path); ++j) {
	    entity *node = get_compound_graph_path_node(path, j);
	    fprintf(F, ".%s", get_entity_name(node));
	    if (is_array_type(get_entity_owner(node)))
	      fprintf(F, "[%d]", get_compound_graph_path_array_index(path, j));
	  }
	  fprintf(F, "\t = ");
	  dump_node_opcode(F, get_compound_ent_value(ent, i));
	}
      }
      fprintf(F, "\n");
    }
  }


  if (verbosity & dump_verbosity_entattrs) {
    fprintf(F, "%s  volatility:  ", prefix);
    switch (get_entity_volatility(ent)) {
      X(volatility_non_volatile);
      X(volatility_is_volatile);
    }

    fprintf(F, "\n%s  peculiarity: %s", prefix, get_peculiarity_string(get_entity_peculiarity(ent)));
    fprintf(F, "\n%s  ld_name: %s", prefix, ent->ld_name ? get_entity_ld_name(ent) : "no yet set");
    fprintf(F, "\n%s  offset:  %d", prefix, get_entity_offset_bits(ent));
    if (is_method_type(get_entity_type(ent))) {
      if (get_entity_irg(ent))   /* can be null */ {
	fprintf(F, "\n%s  irg = %ld", prefix, get_irg_graph_nr(get_entity_irg(ent)));
	if (get_irp_callgraph_state() == irp_callgraph_and_calltree_consistent) {
	  fprintf(F, "\n%s    recursion depth %d", prefix, get_irg_recursion_depth(get_entity_irg(ent)));
	  fprintf(F, "\n%s    loop depth      %d", prefix, get_irg_loop_depth(get_entity_irg(ent)));
	}
      } else {
	fprintf(F, "\n%s  irg = NULL", prefix);
      }
    }
    fprintf(F, "\n");
  }
}
#undef X

void    dump_entity_to_file (FILE *F, entity *ent, unsigned verbosity) {
  dump_entity_to_file_prefix (F, ent, "", verbosity);
  fprintf(F, "\n");
}

void dump_entity (entity *ent) {
  dump_entity_to_file(stdout, ent, dump_verbosity_max);
}

void dump_type_to_file (FILE *F, type *tp, dump_verbosity verbosity) {
  int i;

  if ((is_class_type(tp))       && (verbosity & dump_verbosity_noClassTypes)) return;
  if ((is_struct_type(tp))      && (verbosity & dump_verbosity_noStructTypes)) return;
  if ((is_union_type(tp))       && (verbosity & dump_verbosity_noUnionTypes)) return;
  if ((is_array_type(tp))       && (verbosity & dump_verbosity_noArrayTypes)) return;
  if ((is_pointer_type(tp))     && (verbosity & dump_verbosity_noPointerTypes)) return;
  if ((is_method_type(tp))      && (verbosity & dump_verbosity_noMethodTypes)) return;
  if ((is_primitive_type(tp))   && (verbosity & dump_verbosity_noPrimitiveTypes)) return;
  if ((is_enumeration_type(tp)) && (verbosity & dump_verbosity_noEnumerationTypes)) return;

  fprintf(F, "%s type %s (%ld)", get_tpop_name(get_type_tpop(tp)), get_type_name(tp), get_type_nr(tp));
  if (verbosity & dump_verbosity_onlynames) { fprintf(F, "\n"); return; }

  switch (get_type_tpop_code(tp)) {

  case tpo_class:
    if ((verbosity & dump_verbosity_methods) || (verbosity & dump_verbosity_fields)) {
      fprintf(F, "\n  members: \n");
    }
    for (i = 0; i < get_class_n_members(tp); ++i) {
      entity *mem = get_class_member(tp, i);
      if (((verbosity & dump_verbosity_methods) &&  is_method_type(get_entity_type(mem))) ||
	  ((verbosity & dump_verbosity_fields)  && !is_method_type(get_entity_type(mem)))   ) {
	dump_entity_to_file_prefix(F, mem, "    ", verbosity);
      }
    }
    if (verbosity & dump_verbosity_typeattrs) {
      fprintf(F, "  supertypes: ");
      for (i = 0; i < get_class_n_supertypes(tp); ++i) {
	type *stp = get_class_supertype(tp, i);
	fprintf(F, "\n    %s", get_type_name(stp));
      }
      fprintf(F, "\n  subtypes: ");
      for (i = 0; i < get_class_n_subtypes(tp); ++i) {
	type *stp = get_class_subtype(tp, i);
	fprintf(F, "\n    %s", get_type_name(stp));
      }

      fprintf(F, "\n  peculiarity: %s", get_peculiarity_string(get_class_peculiarity(tp)));
    }
    break;

  case tpo_union:
  case tpo_struct:
    if (verbosity & dump_verbosity_fields) fprintf(F, "\n  members: ");
    for (i = 0; i < get_compound_n_members(tp); ++i) {
      entity *mem = get_compound_member(tp, i);
      if (verbosity & dump_verbosity_fields) {
	dump_entity_to_file_prefix(F, mem, "    ", verbosity);
      }
    }
    break;

  case tpo_pointer: {
    if (verbosity & dump_verbosity_typeattrs) {
      type *tt = get_pointer_points_to_type(tp);
      fprintf(F, "\n  points to %s (%ld)", get_type_name(tt), get_type_nr(tt));
    }

  } break;

  default:
    if (verbosity & dump_verbosity_typeattrs) {
      fprintf(F, ": details not implemented\n");
    }
  }
  fprintf(F, "\n\n");
}

void dump_type(type *tp) {
  dump_type_to_file (stdout, tp, dump_verbosity_max);
}



static FILE *text_open (const char *basename, const char * suffix1, const char *suffix2) {
  FILE *F;
  int len = strlen(basename), i, j;
  char *fname;  /* filename to put the vcg information in */

  if (!basename) assert(basename);
  if (!suffix1) suffix1 = "";
  if (!suffix2) suffix2 = "";

  /* open file for vcg graph */
  fname = malloc (strlen(basename)*2 + strlen(suffix1) + strlen(suffix2) + 5); /* *2: space for excapes. */

  j = 0;
  for (i = 0; i < len; ++i) {  /* replase '/' in the name: escape by @. */
    if (basename[i] == '/') {
      fname[j] = '@'; j++; fname[j] = '1'; j++;
    } else if (basename[i] == '@') {
      fname[j] = '@'; j++; fname[j] = '2'; j++;
    } else {
      fname[j] = basename[i]; j++;
    }
  }
  fname[j] = '\0';
  strcat (fname, suffix1);  /* append file suffix */
  strcat (fname, suffix2);  /* append file suffix */
  strcat (fname, ".txt");   /* append the .txt suffix */

  F = fopen (fname, "w");   /* open file for writing */
  if (!F) {
    assert(0);
  }
  free(fname);

  return F;
}

void dump_types_as_text(unsigned verbosity, const char *suffix) {
  const char *basename;
  FILE *F;
  int i, n_types = get_irp_n_types();

  if (get_irp_prog_ident() == new_id_from_str("no_name_set")) {
    basename = "TextTypes";
  } else {
    basename = get_irp_prog_name();
  }
  F = text_open (basename, suffix, "-types");

  for (i = 0; i < n_types; ++i) {
    type *t = get_irp_type(i);
    dump_type_to_file(F, t, verbosity);
  }

  fclose (F);
}
