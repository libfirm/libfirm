/*
 * Project:     libFIRM / Recoder frontend for libFIRM
 * File name:   firmjni/Dbginfo.c
 * Purpose:     Testing callback.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     26.2.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#include "Dbginfo.h"   /* Generated. */

#include "firm.h"
#include "dbginfo.h"
#include <stdio.h>

static jmethodID pair_id;
static jmethodID sets_id;
static JNIEnv   *my_env_jni_;
static jclass    my_cls_jni_;

struct dbg_info
{
  int idx;
};

static void
my_dbg_info_merge_pair(ir_node *nw, ir_node *old, dbg_action info) {
  if (pair_id) {
    (*my_env_jni_)->CallStaticVoidMethod(my_env_jni_, my_cls_jni_, pair_id, nw, old, info);
  }
}

static void
my_dbg_info_merge_sets(ir_node **new_nodes, int n_new_nodes,
		    ir_node **old_nodes, int n_old_nodes,
		    dbg_action info) {
  if (sets_id) {
    /* Convert argument arrays to java arrays. */
    jintArray j_new_nodes = (*my_env_jni_)->NewIntArray(my_env_jni_, n_new_nodes);
    jintArray j_old_nodes = (*my_env_jni_)->NewIntArray(my_env_jni_, n_old_nodes);
    if (j_new_nodes)
      (*my_env_jni_)->SetIntArrayRegion(my_env_jni_, j_new_nodes, 0, n_new_nodes, (jint *)new_nodes);
    if (j_old_nodes)
      (*my_env_jni_)->SetIntArrayRegion(my_env_jni_, j_old_nodes, 0, n_old_nodes, (jint *)old_nodes);

    (*my_env_jni_)->CallStaticVoidMethod(my_env_jni_, my_cls_jni_, sets_id, j_new_nodes, j_old_nodes, info);
  }
}


void Java_firmjni_Dbginfo_dbgInit (JNIEnv *env_jni_, jclass cls_jni_) {
  pair_id = (*env_jni_)->GetStaticMethodID(env_jni_, cls_jni_, "myJavaDbgInfoMergePair", "(III)V");
  sets_id = (*env_jni_)->GetStaticMethodID(env_jni_, cls_jni_, "myJavaDbgInfoMergeSets", "([I[II)V");
  my_env_jni_ = env_jni_;
  my_cls_jni_ = cls_jni_;

  dbg_init(&my_dbg_info_merge_pair, &my_dbg_info_merge_sets);
}

/** Return the dbg object index of the given node. A new dbg object is created, if needed. */
jint Java_firmjni_Dbginfo_getDbgInfoIdx (JNIEnv *env, jclass clss, jint jnode)
{
  /*
  ir_node *node = (ir_node*) jnode;

  dbg_info *info = get_irn_dbg_info (node);

  if (0 == info) {
	info = get_dbg_info ();
	set_irn_dbg_info (node, info);
  }

  return (info->idx);
  */

  return (0);
}

/** Return the dbg object index of the given node, or <TT>-1</TT> if none has been set. */
jint Java_firmjni_Dbginfo_doGetDbgInfoIdx (JNIEnv *env, jclass clss, jint jnode)
{
  /*
  ir_node *node = (ir_node*) jnode;

  dbg_info *info = get_irn_dbg_info (node);

  if (0 == info) {
	return (-1);
  }

  return (info->idx);
  */

  return (0);
}
