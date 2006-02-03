/*
 * Project:     libFIRM
 * File name:   ir/debug/debugger.c
 * Purpose:     Helper function for integerated debug support
 * Author:      Michael Beck
 * Modified by:
 * Created:     2005
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001-2005 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef NDEBUG

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>
#include <signal.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ctype.h>

#include "set.h"
#include "ident.h"
#include "irhooks.h"
#include "irgraph_t.h"
#include "entity_t.h"
#include "irprintf.h"
#include "debug.h"

#ifdef _WIN32
/* Break into the debugger. The Win32 way. */
void firm_debug_break(void) {
  DebugBreak();
}
#elif defined(__GNUC__) && (defined(__i386__) || defined(__x86_64))
/* Break into the debugger. The ia32/x86_64 way under GCC. */
void firm_debug_break(void) {
  __asm__ __volatile__("int3");
}
#else
/* Break into the debugger. Poor Unix way. */
void firm_debug_break(void) {
  raise(SIGINT);
}
#endif /* _WIN32 */

/** supported breakpoint kinds */
typedef enum {
  BP_NR  = 'n',   /**< break on node number. */
  BP_IDENT = 'i'    /**< break on ident. */
} bp_kind;

/**
 * Reasons for node number breakpoints.
 */
typedef enum _bp_reasons_t {
  BP_ON_NEW_NODE,    /**< break if node with number is created */
  BP_ON_REPLACE,     /**< break if node with number is replaced */
  BP_ON_LOWER,       /**< break if node with number is lowered */
  BP_ON_REMIRG,      /**< break if an IRG is removed */
  BP_ON_NEW_ENT,     /**< break if a new entity is created */
  BP_ON_NEW_TYPE,    /**< break if a new type is created */
  BP_MAX_REASON
} bp_reasons_t;

/** A breakpoint. */
typedef struct _breakpoint {
  bp_kind      kind;        /**< the kind of this break point */
  unsigned     bpnr;        /**< break point number */
  int          active;      /**< non-zero, if this break point is active */
  bp_reasons_t reason;      /**< reason for the breakpoint */
  struct _breakpoint *next; /**< link to the next one */
} breakpoint;

/** A number breakpoint. */
typedef struct {
  breakpoint   bp;       /**< the breakpoint data */
  long         nr;       /**< the node number */
} bp_nr_t;

/** calculate the hash value for a node breakpoint */
#define HASH_NR_BP(key) (((key).nr << 2) ^ (key).bp.reason)

/** A ident breakpoint. */
typedef struct {
  breakpoint   bp;       /**< the breakpoint data */
  ident        *id;      /**< the ident */
} bp_ident_t;

/** calculate the hash value for an ident breakpoint */
#define HASH_IDENT_BP(key) (HASH_PTR((key).id) ^ (key).bp.reason)

/** The set containing the breakpoints on node numbers. */
static set *bp_numbers;

/** The set containing the breakpoints on idents. */
static set *bp_idents;

/**< the list of all breakpoints */
static breakpoint *bp_list;

/** number of the current break point */
static unsigned bp_num = 0;

/** set if break on init command was issued. */
static int break_on_init = 0;

/** the hook entries for the Firm debugger module. */
static hook_entry_t debugger_hooks[hook_last];

/** number of active breakpoints to maintain hooks. */
static unsigned num_active_bp[BP_MAX_REASON];

/**
 * The debug message buffer
 */
static char firm_dbg_msg_buf[2048];

/**
 * If set, the debug extension writes all output to the
 * firm_dbg_msg_buf buffer
 */
static int redir_output = 0;

/**
 * Is set to one, if the debug extension is active
 */
static int is_active = 0;

/** hook the hook h with function fkt. */
#define HOOK(h, fkt) \
do {                                    \
  debugger_hooks[h].hook._##h = fkt;    \
  register_hook(h, &debugger_hooks[h]); \
} while(0)

/** unhook the hook h */
#define UNHOOK(h) \
do {                                      \
  unregister_hook(h, &debugger_hooks[h]); \
  debugger_hooks[h].hook._##h = NULL;     \
} while(0)

/** returns non-zero if a entry hook h is used */
#define IS_HOOKED(h) (debugger_hooks[h].hook._##h != NULL)

/* some macros needed to create the info string */
#define _DBG_VERSION(major, minor)  #major "." #minor
#define DBG_VERSION(major, minor)   _DBG_VERSION(major, minor)
#define API_VERSION(major, minor)   "API:" DBG_VERSION(major, minor)

/* the API version: change if needed */
#define FIRM_DBG_MAJOR  1
#define FIRM_DBG_MINOR  0

/** for automatic detection of the debug extension */
static const char *firm_debug_info_string =
  API_VERSION(FIRM_DBG_MAJOR, FIRM_DBG_MINOR)
  ;

/**
 * Returns non-zero, if the debug extension is active
 */
int firm_debug_active(void) {
  return is_active;
}

/**
 * reset the debug text buffer
 */
static void reset_dbg_buf(void) {
  firm_dbg_msg_buf[0] = '\0';
}

/**
 * Add text to the debug text buffer
 */
static void add_to_dbg_buf(const char *buf) {
  strncat(firm_dbg_msg_buf, buf, sizeof(firm_dbg_msg_buf));
}

/**
 * Return the content of the debug text buffer.
 *
 * To be called from the debugger.
 */
const char *firm_debug_text(void) {
  return firm_dbg_msg_buf;
}

/**
 * debug output
 */
static void dbg_printf(const char *fmt, ...)
{
  char buf[1024];

  va_list args;
  va_start(args, fmt);

  if (fmt[0] != '+')
    reset_dbg_buf();
  else
    ++fmt;

  ir_vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(args);

  if (redir_output)
    add_to_dbg_buf(buf);
  else
    puts(buf);
}

/**
 * A new node is created.
 *
 * @param ctx   the hook context
 * @param irg   the IR graph on which the node is created
 * @param node  the new IR node that was created
 */
static void dbg_new_node(void *ctx, ir_graph *irg, ir_node *node)
{
  bp_nr_t key, *elem;

  key.nr        = get_irn_node_nr(node);
  key.bp.reason = BP_ON_NEW_NODE;

  elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
  if (elem && elem->bp.active) {
    dbg_printf("Firm BP %u reached, %+F created\n", elem->bp.bpnr, node);
    firm_debug_break();
  }
}

/**
 * A node is replaced.
 *
 * @param ctx   the hook context
 * @param old   the IR node the is replaced
 * @param nw    the new IR node that will replace old
 */
static void dbg_replace(void *ctx, ir_node *old, ir_node *nw)
{
  bp_nr_t key, *elem;

  key.nr        = get_irn_node_nr(old);
  key.bp.reason = BP_ON_REPLACE;

  elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
  if (elem && elem->bp.active) {
    dbg_printf("Firm BP %u reached, %+F will be replaced by %+F\n", elem->bp.bpnr, old, nw);
    firm_debug_break();
  }
}

/**
 * A new node is lowered.
 *
 * @param ctx   the hook context
 * @param node  the new IR node that will be lowered
 */
static void dbg_lower(void *ctx, ir_node *node)
{
  bp_nr_t key, *elem;

  key.nr        = get_irn_node_nr(node);
  key.bp.reason = BP_ON_LOWER;

  elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
  if (elem && elem->bp.active) {
    dbg_printf("Firm BP %u reached, %+F will be lowered\n", elem->bp.bpnr, node);
    firm_debug_break();
  }
}

/**
 * A graph will be deleted.
 *
 * @param ctx   the hook context
 * @param irg   the IR graph that will be deleted
 */
static void dbg_free_graph(void *ctx, ir_graph *irg)
{
  {
    bp_nr_t key, *elem;
    key.nr        = get_irg_graph_nr(irg);
    key.bp.reason = BP_ON_REMIRG;

    elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
    if (elem && elem->bp.active) {
      ir_printf("Firm BP %u reached, %+F will be deleted\n", elem->bp.bpnr, irg);
      firm_debug_break();
    }
  }
  {
    bp_ident_t key, *elem;
    entity *ent = get_irg_entity(irg);

    if (! ent)
      return;

    key.id        = get_entity_ident(ent);
    key.bp.reason = BP_ON_REMIRG;

    elem = set_find(bp_idents, &key, sizeof(key), HASH_IDENT_BP(key));
    if (elem && elem->bp.active) {
      dbg_printf("Firm BP %u reached, %+F will be deleted\n", elem->bp.bpnr, ent);
      firm_debug_break();
    }
  }
}

/**
 * An entity was created.
 *
 * @param ctx   the hook context
 * @param ent   the newly created entity
 */
static void dbg_new_entity(void *ctx, entity *ent)
{
  {
    bp_ident_t key, *elem;

    key.id        = get_entity_ident(ent);
    key.bp.reason = BP_ON_NEW_ENT;

    elem = set_find(bp_idents, &key, sizeof(key), HASH_IDENT_BP(key));
    if (elem && elem->bp.active) {
      ir_printf("Firm BP %u reached, %+F was created\n", elem->bp.bpnr, ent);
      firm_debug_break();
    }
  }
  {
    bp_nr_t key, *elem;

    key.nr        = get_entity_nr(ent);
    key.bp.reason = BP_ON_NEW_ENT;

    elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
    if (elem && elem->bp.active) {
      dbg_printf("Firm BP %u reached, %+F was created\n", elem->bp.bpnr, ent);
      firm_debug_break();
    }
  }
}

/**
 * A type was created.
 *
 * @param ctx   the hook context
 * @param tp    the newly created type
 */
static void dbg_new_type(void *ctx, ir_type *tp)
{
  {
    bp_nr_t key, *elem;

    key.nr        = get_type_nr(tp);
    key.bp.reason = BP_ON_NEW_TYPE;

    elem = set_find(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));
    if (elem && elem->bp.active) {
      ir_printf("Firm BP %u reached, %+F was created\n", elem->bp.bpnr, tp);
      firm_debug_break();
    }
  }
  {
    bp_ident_t key, *elem;

    key.id        = get_type_ident(tp);
    key.bp.reason = BP_ON_NEW_TYPE;

    elem = set_find(bp_idents, &key, sizeof(key), HASH_IDENT_BP(key));
    if (elem && elem->bp.active) {
      dbg_printf("Firm BP %u reached, %+F was created\n", elem->bp.bpnr, tp);
      firm_debug_break();
    }
  }
}

/**
 * return the reason string.
 */
static const char *reason_str(bp_reasons_t reason)
{
  switch (reason) {
  case BP_ON_NEW_NODE: return "node creation";
  case BP_ON_REPLACE:  return "node replace";
  case BP_ON_LOWER:    return "node lowering";
  case BP_ON_REMIRG:   return "removing IRG";
  case BP_ON_NEW_ENT:  return "entity creation";
  case BP_ON_NEW_TYPE: return "type creation";
  default:             assert(0);
  }
  return "unknown";
}

/**
 * Compare two number breakpoints
 */
static int cmp_nr_bp(const void *elt, const void *key, size_t size)
{
  const bp_nr_t *e1 = elt;
  const bp_nr_t *e2 = key;

  return (e1->nr - e2->nr) | (e1->bp.reason - e2->bp.reason);
}

/**
 * Compare two ident breakpoints
 */
static int cmp_ident_bp(const void *elt, const void *key, size_t size)
{
  const bp_ident_t *e1 = elt;
  const bp_ident_t *e2 = key;

  return (e1->id != e2->id) | (e1->bp.reason - e2->bp.reason);
}

/**
 * update the hooks
 */
static void update_hooks(breakpoint *bp)
{
#define CASE_ON(a, b)  case a: if (! IS_HOOKED(hook_##b)) HOOK(hook_##b, dbg_##b); break
#define CASE_OFF(a, b) case a: if (IS_HOOKED(hook_##b)) UNHOOK(hook_##b); break

  if (bp->active)
    ++num_active_bp[bp->reason];
  else
    --num_active_bp[bp->reason];

  if (num_active_bp[bp->reason] > 0) {
    /* register the hooks on demand */
    switch (bp->reason) {
    CASE_ON(BP_ON_NEW_NODE, new_node);
    CASE_ON(BP_ON_REPLACE, replace);
    CASE_ON(BP_ON_LOWER, lower);
    CASE_ON(BP_ON_REMIRG, free_graph);
    CASE_ON(BP_ON_NEW_ENT, new_entity);
    CASE_ON(BP_ON_NEW_TYPE, new_type);
    default:
      ;
    }
  }
  else {
    /* unregister the hook on demand */
    switch (bp->reason) {
    CASE_OFF(BP_ON_NEW_NODE, new_node);
    CASE_OFF(BP_ON_REPLACE, replace);
    CASE_OFF(BP_ON_LOWER, lower);
    CASE_OFF(BP_ON_REMIRG, free_graph);
    CASE_OFF(BP_ON_NEW_ENT, new_entity);
    CASE_OFF(BP_ON_NEW_TYPE, new_type);
    default:
      ;
    }
  }
#undef CASE_ON
#undef CASE_OFF
}

/**
 * Break if nr is reached.
 */
static void break_on_nr(long nr, bp_reasons_t reason)
{
  bp_nr_t key, *elem;

  key.bp.kind   = BP_NR;
  key.bp.bpnr   = 0;
  key.bp.active = 1;
  key.bp.reason = reason;
  key.nr        = nr;

  elem = set_insert(bp_numbers, &key, sizeof(key), HASH_NR_BP(key));

  if (elem->bp.bpnr == 0) {
    /* new break point */
    elem->bp.bpnr = ++bp_num;
    elem->bp.next = bp_list;
    bp_list = &elem->bp;

    dbg_printf("Firm BP %u: %s of Nr %ld\n", elem->bp.bpnr, reason_str(reason), nr);

    update_hooks(&elem->bp);
  }
}

/**
 * Break if ident name is reached.
 */
static void break_on_ident(const char *name, bp_reasons_t reason) {
  bp_ident_t key, *elem;

  key.bp.kind   = BP_IDENT;
  key.bp.bpnr   = 0;
  key.bp.active = 1;
  key.bp.reason = reason;
  key.id        = new_id_from_str(name);

  elem = set_insert(bp_idents, &key, sizeof(key), HASH_IDENT_BP(key));

  if (elem->bp.bpnr == 0) {
    /* new break point */
    elem->bp.bpnr = ++bp_num;
    elem->bp.next = bp_list;
    bp_list = &elem->bp;

    dbg_printf("Firm BP %u: %s of ident \"%s\"\n", elem->bp.bpnr, reason_str(reason), name);

    update_hooks(&elem->bp);
  }
}

/**
 * Sets/resets the active flag of breakpoint bp.
 */
static void bp_activate(unsigned bp, int active)
{
  breakpoint *p;

  for (p = bp_list; p; p = p->next) {
    if (p->bpnr == bp) {
      if (p->active != active) {
        p->active = active;
        update_hooks(p);
      }

      dbg_printf("Firm BP %u is now %s\n", bp, active ? "enabled" : "disabled");
      return;
    }
  }
  dbg_printf("Error: Firm BP %u not exists.\n", bp);
}


/**
 * Show a list of supported commands
 */
static void show_commands(void) {
  dbg_printf("Internal Firm debugger extension $Revision$ commands:\n"
    ".init                  break after initialization\n"
    ".create nr             break if node nr was created\n"
    ".replace nr            break if node nr is replaced by another node\n"
    ".lower nr              break before node nr is lowered\n"
    ".remirg nr|name        break if the irg of nr or entity name is deleted\n"
    ".newent nr|name        break if the entity nr or name was created\n"
    ".newtype nr|name       break if the type nr or name was created\n"
    ".bp                    show all breakpoints\n"
    ".enable nr             enable breakpoint nr\n"
    ".disable nr            disable breakpoint nr\n"
    ".setmask name lvl      sets the debug module name to level lvl\n"
    ".setoutfile name file  redirects debug output of module name to file\n"
    ".help                  list all commands\n"
  );
}

/**
 * Shows all Firm breakpoints.
 */
static void show_bp(void) {
  breakpoint *p;
  bp_nr_t  *node_p;
  bp_ident_t *ident_p;
  int have_one = 0;

  dbg_printf("Firm Breakpoints:");
  for (p = bp_list; p; p = p->next) {
    have_one = 1;
    dbg_printf("+\n  BP %u: ", p->bpnr);

    switch (p->kind) {
    case BP_NR:
      node_p = (bp_nr_t *)p;
      dbg_printf("%s of Nr %ld ", reason_str(p->reason), node_p->nr);
      break;

    case BP_IDENT:
      ident_p = (bp_ident_t *)p;
      dbg_printf("+%s of ident \"%s\" ", reason_str(p->reason), get_id_str(ident_p->id));
      break;
    }

    dbg_printf(p->active ? "+enabled" : "+disabled");
  }
  dbg_printf(have_one ? "+\n" : "+ NONE\n");
}

/**
 * firm_dbg_register() expects that the name is stored persistent.
 * So we need this little helper function
 */
static firm_dbg_module_t *dbg_register(const char *name) {
  ident *id = new_id_from_str(name);

  return firm_dbg_register(get_id_str(id));
}

/**
 * Sets the debug mask of module name to lvl
 */
static void set_dbg_level(const char *name, unsigned lvl)
{
  firm_dbg_module_t *module = dbg_register(name);

  firm_dbg_set_mask(module, lvl);

  dbg_printf("Setting debug mask of module %s to %u\n", name, lvl);
}

/**
 * Redirects the debug output of module name to fname
 */
static void set_dbg_outfile(const char *name, const char *fname)
{
  firm_dbg_module_t *module = dbg_register(name);
  FILE *f = fopen(fname, "w");

  if (! f) {
    perror(fname);
    return;
  }

  firm_dbg_set_file(module, f);
  dbg_printf("Redirecting debug output of module %s to file %s\n", name, fname);
}

/**
 * High level function to use from debugger interface
 *
 * Supported commands:
 *  .create nr    break if node nr was created
 *  .help         list all commands
 */
void firm_debug(const char *cmd) {
  long nr;
  unsigned bp;
  char name[1024], fname[1024];
  unsigned lvl;

  while (isspace(*cmd)) ++cmd;

  if (sscanf(cmd, ".create %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_NEW_NODE);
  }
  else if (sscanf(cmd, ".replace %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_REPLACE);
  }
  else if (sscanf(cmd, ".lower %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_LOWER);
  }
  else if (sscanf(cmd, ".remirg %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_REMIRG);
  }
  else if (sscanf(cmd, ".remirg %s\n", name) == 1) {
    break_on_ident(name, BP_ON_REMIRG);
  }
  else if (sscanf(cmd, ".newent %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_NEW_ENT);
  }
  else if (sscanf(cmd, ".newent %s\n", name) == 1) {
    break_on_ident(name, BP_ON_NEW_ENT);
  }
  else if (sscanf(cmd, ".newtype %ld\n", &nr) == 1) {
    break_on_nr(nr, BP_ON_NEW_TYPE);
  }
  else if (sscanf(cmd, ".newtype %s\n", name) == 1) {
    break_on_ident(name, BP_ON_NEW_TYPE);
  }
  else if (strcmp(cmd, ".init") == 0)
    break_on_init = 1;
  else if (strcmp(cmd, ".bp") == 0)
    show_bp();
  else if (sscanf(cmd, ".enable %u", &bp) == 1)
    bp_activate(bp, 1);
  else if (sscanf(cmd, ".disable %u", &bp) == 1)
    bp_activate(bp, 0);
  else if (sscanf(cmd, ".setmask %s %u\n", name, &lvl) == 2)
    set_dbg_level(name, lvl);
  else if (sscanf(cmd, ".setoutfile %s %s\n", name, fname) == 2)
    set_dbg_outfile(name, fname);
  else {
    show_commands();
  }
}

/* creates the debugger tables */
void firm_init_debugger(void)
{
  char *env;

  bp_numbers = new_set(cmp_nr_bp, 8);
  bp_idents  = new_set(cmp_ident_bp, 8);

  env = getenv("FIRMDBG");

  is_active = 1;

  if (env)
    firm_debug(env);

  if (break_on_init)
    firm_debug_break();
}

#else

/* some picky compiler do not allow empty files */
static int _firm_only_that_you_can_compile_with_NDEBUG_defined;

#endif /* NDEBUG */

/**
 * @page debugger   The Firm debugger extension
 *
 * Firm contains a debugger extension. This allows to set debugger breakpoints
 * an various events.
 * The extension uses a text interface which can be accessed from most debuggers.
 *
 * @section sec_cmd Supported commands
 *
 * The following commands are currently supported:
 * @b .init
 *
 * Break immediately after the debugger extension was initialized.
 * Typically this command is used in the environment to stop the execution
 * of a Firm compiler right after the initialization, like this:
 *
 * $export FIRMDBG=".init"
 *
 *
 * @b .create nr
 *
 * Break if a new IR-node with node number nr was created.
 * Typically used to find the place where wrong nodes are created.
 *
 * @b .replace nr
 *
 * Break before IR-node with node number nr is replaced by another node.
 *
 * @b .lower nr
 *
 * Break before IR-node with node number nr is lowered.
 *
 * @b .remirg nr
 *
 * Break if the irg with graph number nr is deleted.
 *
 * @b .remirg name
 *
 * Break if the irg of entity name is deleted.
 *
 * @b .newent nr
 *
 * Break if the entity with number nr was created.
 *
 * @b .newent name
 *
 * Break if the entity name was created.
 *
 * @b .newtype nr
 *
 * Break if the type with number nr was created.
 *
 * @b .newtype name
 *
 * Break if the type name was created.
 *
 * @b .bp
 *
 * Show all Firm internal breakpoints.
 *
 * @b .enable nr
 *
 * Enables breakpoint nr.
 *
 * @b .disable nr
 *
 * Disables breakpoint nr.
 *
 * @b .setmask name lvl
 *
 * Sets the debug module name to level lvl.
 *
 * @b .setoutfile name file
 *
 * Redirects debug output of module name to file.
 *
 * @b .help
 *
 * List all commands.
 *
 *
 * The Firm debugger extension can be accessed using the function firm_debug().
 * The following example shows how to set a creation breakpoint in GDB when
 * node 2101 is created.
 *
 * -# set FIRMDBG=".init"
 * -# start gdb with your compiler
 * -# after gdb breaks, issue
 *
 * p firm_debug(".create 2101")
 *
 * On the console the following text should be issued:
 *
 * Firm BP 1: creation of Node 2101
 *
 *
 * @section gdb_macro GDB macro
 *
 * Add the following to your .gdbinit file:
 * @code
 #
 # define firm "cmd"  Firm debugger extension
 #
 define firm
 p firm_debug($arg0)
 end
 * @endcode
 *
 * Then, all Firm debugger extension commands can be access in the gdb
 * console using the firm prefix, eg.:
 *
 * firm ".create 2101"
 * firm ".help"
 */
