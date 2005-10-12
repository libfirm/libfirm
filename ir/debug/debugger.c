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

#include "set.h"
#include "irhooks.h"
#include "irprintf.h"

#ifdef _WIN32
/* Win32 support */

/** Break into the debugger */
static void firm_debug_break(void) {
  DebugBreak();
}

#else

/** Break into the debugger */
static void firm_debug_break(void) {
  raise(SIGINT);
}

#endif /* _WIN32 */

/** supported breakpoint kinds */
typedef enum {
  BP_CREATION = 0    /**< break on creation. */
} bp_kind;

/** A breakpoint. */
typedef struct _breakpoint {
  bp_kind   kind;           /**< the kind of this break point */
  unsigned  bpnr;           /**< break point number */
  int       active;         /**< non-zero, if this break point is active */
  struct _breakpoint *next; /**< link to the next one */
} breakpoint;

/** A creation breakpoint. */
typedef struct {
  breakpoint  bp; /**< the breakpoint data */
  long        nr; /**< the node number */
} bp_create_t;

/** The set containing the creation breakpoints. */
static set *bp_creations;

/**< the list of all breakpoints */
static breakpoint *bp_list;

/** number of the current break point */
static unsigned bp_num = 0;

/** set if break on init command was issued. */
static int break_on_init = 0;

/**
 * Compare two creation breakpoints
 */
static int cmp_create_bp(const void *elt, const void *key, size_t size)
{
  const bp_create_t *e1 = elt;
  const bp_create_t *e2 = key;

  return e1->nr - e2->nr;
}

/**
 * Break if node nr is created.
 */
static void break_on_creation(long nr)
{
  bp_create_t key, *elem;

  key.bp.kind   = BP_CREATION;
  key.bp.bpnr   = 0;
  key.bp.active = 1;
  key.nr        = nr;

  elem = set_insert(bp_creations, &key, sizeof(key), nr);

  if (elem->bp.bpnr == 0) {
    /* new break point */
    elem->bp.bpnr = ++bp_num;
    elem->bp.next = bp_list;
    bp_list = &elem->bp;

    printf("Firm BP %u: creation of Node %ld\n", elem->bp.bpnr, nr);
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
      p->bpnr = active;

      printf("Firm BP %u is now %s\n", bp, active ? "enabled" : "disabled");
      return;
    }
  }
  printf("Error: Firm BP %u not exists.\n", bp);
}


/**
 * Show a list of supported commands
 */
static void show_commands(void) {
  printf("Internal Firm debugger extension commands:\n"
    ".init         break after initialization\n"
    ".create nr    break if node nr was created\n"
    ".bp           show all breakpoints\n"
    ".enable nr    enable breakpoint nr\n"
    ".disable nr   disable breakpoint nr\n"
    ".help         list all commands\n"
  );
}

/**
 * Shows all Firm breakpoints.
 */
static void show_bp(void) {
  breakpoint *p;

  for (p = bp_list; p; p = p->next) {
    printf("Firm BP %u ", p->bpnr);

    switch (p->kind) {
    case BP_CREATION:
      printf("creation of node %ld ", ((bp_create_t *)p)->nr);
      break;
    }

    printf(p->active ? "enabled\n" : "disabled\n");
  }
}

/**
 * High level function to use from debugger interface
 *
 * Supported commands:
 *  .create nr    break if node nr was created
 *  .help         list all commands
 */
void firm_break(const char *cmd) {
  long nr;
  unsigned bp;

  while (isspace(*cmd)) ++cmd;

  if (sscanf(cmd, ".create %ld\n", &nr) == 1) {
    break_on_creation(nr);
  }
  else if (strcmp(cmd, ".init") == 0)
    break_on_init = 1;
  else if (strcmp(cmd, ".bp") == 0)
    show_bp();
  else if (sscanf(cmd, ".enable %u", &bp) == 1)
    bp_activate(bp, 1);
  else if (sscanf(cmd, ".disable %u", &bp) == 1)
    bp_activate(bp, 0);
  else {
    show_commands();
  }
}

/** the hook entries for the Firm debugger module */
static hook_entry_t debugger_hooks[hook_last];

/**
 * A new node is created.
 *
 * @param ctx   the hook context
 * @param irg   the IR graph on which the node is created
 * @param node  the new IR node that was created
 */
static void dbg_new_node(void *ctx, ir_graph *irg, ir_node *node)
{
  bp_create_t key, *elem;

  key.nr   = get_irn_node_nr(node);

  elem = set_find(bp_creations, &key, sizeof(key), key.nr);
  if (elem && elem->bp.active) {
    ir_printf("Firm BP %u reached, %+F created\n", elem->bp.bpnr, node);
    firm_debug_break();
  }
}

#define HOOK(h, fkt) \
  debugger_hooks[h].hook._##h = fkt; register_hook(h, &debugger_hooks[h])

/* creates the debugger tables */
void firm_init_debugger(void)
{
  char *env;

  bp_creations = new_set(cmp_create_bp, 8);

  /* register the hooks */
  HOOK(hook_new_node,                         dbg_new_node);

  env = getenv("FIRMDBG");

  if (env)
    firm_break(env);

  if (break_on_init)
    firm_debug_break();
}

#endif /* NDEBUG */

/**
 * @page debugger   The Firm debugger extension.
 *
 * Firm contains a debugger extension. This allows to set debugger breakpoints
 * an various events.
 * The extension uses a text interface which can be access in the debugger.
 *
 * The following commands are currently supported:
 *
 * .init
 *
 * Break immediately after the debugger extension was initialized.
 * Typically this command is used in the environment to stop the execution
 * of a Firm compiler right after the initialization, like this:
 *
 * $export FIRMDBG=".init"
 *
 *
 * .create nr
 *
 * Break if a new IR-node with node number nr was created.
 * Typically used to find the place where wrong nodes are created.
 *
 * .bp
 *
 * Show all Firm internal breakpoints.
 *
 * .enable nr
 *
 * Enables breakpoint nr.
 *
 * .disable nr
 *
 * Disables breakpoint nr.
 *
 * .help
 *
 * List all commands.
 *
 *
 * The Firm debugger extension is access using the function firm_break().
 * The following example shows how to set a creating breakpoint in GDB when
 * node 2101 is created.
 *
 * 1.) set FRIMDBG=".init"
 * 2.) start gdb with your compiler
 * 3.) after gdb breaks, issue
 *
 * p firm_debug(".create 2101")
 *
 * On the console the following text should be issued:
 *
 * Firm BP 1: creation of Node 2101
 */
