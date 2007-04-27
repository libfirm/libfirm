/*
 * Copyright (C) 1995-2007 University of Karlsruhe.  All right reserved.
 *
 * This file is part of libFirm.
 *
 * This file may be distributed and/or modified under the terms of the
 * GNU General Public License version 2 as published by the Free Software
 * Foundation and appearing in the file LICENSE.GPL included in the
 * packaging of this file.
 *
 * Licensees holding valid libFirm Professional Edition licenses may use
 * this file in accordance with the libFirm Commercial License.
 * Agreement provided with the Software.
 *
 * This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
 * WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.
 */


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <libcore/lc_opts.h>
#include <libcore/lc_opts_enum.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#include <signal.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "bejavacoal.h"
#include "irtools.h"
#include "bemodule.h"

#ifdef WITH_JVM

/* Path to the jar file. A little OS dependent convenience. */
#ifdef _WIN32
static char jar_file[512] = "y:\\user\\hack\\public\\coal.jar";
#else
static char jar_file[512] = "/ben/hack/public/coal.jar";
#endif

static char cls_name[256] = "coalescing/mst/safe/Algo";

/* Name of the JVM dll/so */
static char jvm_lib[512] = { 0 };

static const lc_opt_table_entry_t options[] = {
	LC_OPT_ENT_STR      ("jvm",  "absolute path to jvm dll",                    jvm_lib, sizeof(jvm_lib)),
	LC_OPT_ENT_STR      ("jar",  "jar file of the coalescer",                   jar_file, sizeof(jar_file)),
	LC_OPT_ENT_STR      ("cls",  "name of the class providing the factory",     cls_name, sizeof(cls_name)),
	{ NULL }
};

void be_init_javacoal(void)
{
	lc_opt_entry_t *be_grp = lc_opt_get_grp(firm_opt_get_root(), "be");
	lc_opt_entry_t *ra_grp = lc_opt_get_grp(be_grp, "ra");
	lc_opt_entry_t *chordal_grp = lc_opt_get_grp(ra_grp, "chordal");
	lc_opt_entry_t *jc_grp = lc_opt_get_grp(chordal_grp, "jc");
	lc_opt_add_table(jc_grp, options);
}

BE_REGISTER_MODULE_CONSTRUCTOR(be_init_javacoal);

#include <jni.h>

typedef struct _jni_env_t {
	JavaVM *jvm;
	JNIEnv *jni;
} jni_env_t;

/*

	Ugly code to retrieve the JVM dll/so file.

*/

#ifdef _WIN32
/* Win32 version */
static void *find_jvm_symbol(const char *vmlibpath, const char *sym)
{
	HINSTANCE hVM = LoadLibrary(vmlibpath);
	return hVM ? GetProcAddress(hVM, sym) : NULL;
}

#define JRE_KEY "SOFTWARE\\JavaSoft\\Java Development Kit"

static char *locate_jvm_lib(char *path, size_t path_len)
{
	char version[32];
	char buf[256];
	DWORD version_len = sizeof(version);
	DWORD dwPathLen = path_len;
	HKEY hKey;

	RegOpenKeyEx(HKEY_LOCAL_MACHINE, JRE_KEY, 0, KEY_QUERY_VALUE, &hKey);
	RegQueryValueEx(hKey, "CurrentVersion", NULL, NULL, (LPBYTE) version, &version_len);
	RegCloseKey(hKey);

	_snprintf(buf, sizeof(buf), JRE_KEY "\\%s", version);
	RegOpenKeyEx(HKEY_LOCAL_MACHINE, buf, 0, KEY_QUERY_VALUE, &hKey);
	RegQueryValueEx(hKey, "JavaHome", NULL, NULL, (LPBYTE) path, &dwPathLen);
	RegCloseKey(hKey);

	strncat(path, "\\jre\\bin\\server\\jvm.dll", path_len);
	return path;
}

#else
/* Unix version */
static void *find_jvm_symbol(const char *vmlibpath, const char *sym)
{
	void *libVM = dlopen(vmlibpath, RTLD_LAZY);
	return libVM ? dlsym(libVM, sym) : NULL;
}

static char *locate_jvm_lib(char *path, size_t n)
{
	return NULL;
}
#endif

static int start_vm(jni_env_t *env, int argc, char *argv[])
{
	int i;
	long ret;
	JavaVMInitArgs args;
	JavaVMOption *opts;

	long (JNICALL * create_func)(JavaVM **, void **, void *) = find_jvm_symbol(jvm_lib, "JNI_CreateJavaVM");

	if(!create_func) {
		fprintf(stderr, "could not find JVM creation function\n");
		exit(1);
	}

	memset(&args, 0, sizeof(args));
	opts = malloc(argc * sizeof(opts[0]));
	for(i = 0; i < argc; ++i) {
		opts[i].optionString = argv[i];
		opts[i].extraInfo    = NULL;
	}

	args.version  = JNI_VERSION_1_4;
	args.nOptions = argc;
	args.options  = opts;
	args.ignoreUnrecognized = JNI_FALSE;

	ret = create_func(&env->jvm, (void **) &env->jni, &args);
	free(opts);
	if(ret != JNI_OK) {
		fprintf(stderr, "JNI_CreateJavaVM returned errrocode %ld\n" , ret);
		return 0;
	}

	return 1;
}

static void stop_vm(jni_env_t *env)
{
	JavaVM *jvm = env->jvm;
	(*jvm)->DetachCurrentThread(jvm);
	(*jvm)->DestroyJavaVM(jvm);
}

static int jvm_inited = 0;
static jni_env_t env;
void (*old_int_handler)(int);
void (*old_abrt_handler)(int);

static void sig_jvm_destroy_at_exit(int signal)
{
	if(jvm_inited)
		stop_vm(&env);

	switch(signal) {
	case SIGABRT:
		old_abrt_handler(signal);
		break;
	case SIGINT:
		old_int_handler(signal);
		break;
	default:;
	}
}

static void jvm_destroy_at_exit(void)
{
	sig_jvm_destroy_at_exit(0);
}

static jni_env_t *get_jvm(void)
{
	char cp_param[512];
	char *args[1];

	if(!jvm_inited) {
		/* Find the dll */
		if(strlen(jvm_lib) == 0) {
			if(!locate_jvm_lib(jvm_lib, sizeof(jvm_lib))) {
				fprintf(stderr, "could not find jvm library\n");
				exit(1);
			}
		}

		snprintf(cp_param, sizeof(cp_param), "-Djava.class.path=%s", jar_file);
		args[0] = cp_param;
		if(!start_vm(&env, sizeof(args) / sizeof(args[0]), args)) {
			fprintf(stderr, "Couldn't initialize java VM\n");
			abort();
		}
		jvm_inited = 1;
		old_int_handler  = signal(SIGINT,  sig_jvm_destroy_at_exit);
		old_abrt_handler = signal(SIGABRT, sig_jvm_destroy_at_exit);
		atexit(jvm_destroy_at_exit);
	}

	return &env;
}


static void check(jni_env_t *env, const char *file, int line)
{
	JNIEnv *jni = env->jni;
	jboolean exc = (*jni)->ExceptionCheck(jni);
	if(exc) {
		fprintf(stderr, "%s:%d: ", file, line);
		(*jni)->ExceptionDescribe(jni);
		(*jni)->ExceptionClear(jni);
		stop_vm(env);
		abort();
	}
}

#define CHECK(env) check(env, __FILE__, __LINE__)

enum {
	mth_add_int_edge,
	mth_add_aff_edge,
	mth_set_color,
	//mth_set_debug,
	mth_get_color,
	mth_forbid_color,
	mth_coalesce,
	mth_dump,
	mth_finish,
	mth_last
};

struct _mth_info_t {
	const char *name;
	const char *sig;
};

static const struct _mth_info_t mthis[mth_last] = {
	{ "addIntEdge",  "(II)V"                   }, /* public void addIntEdge(int, int); */
	{ "addAffEdge",  "(III)V"                  }, /* public void addAffEdge(int, int, int); */
	{ "setColor",    "(II)V"                   }, /* public void setColor(int, int); */
	//{ "setDebug",    "(ILjava/lang/String;)V"  }, /* public void setDebug(int, String); */
	{ "getColor",    "(I)I"                    }, /* public int getColor(int); */
	{ "forbidColor", "(II)V"                   }, /* public void forbidColor(int, int); */
	{ "coalesce",    "()V"                     }, /* public void coalesce(); */
	{ "dump",        "(Ljava/lang/String;)V"   }, /* public void dump(String); */
	{ "finish",      "()V"                     }  /* public void finish(); */
};

/* public static coalescing.Extern createExtern(java.lang.String, int, int, int); */
static const struct _mth_info_t mthi_factory = {
	"createExtern", "(Ljava/lang/String;III)Lcoalescing/Extern;"
};

struct _be_java_coal_t {
	jni_env_t *env;
	jclass    cls;
	jobject   obj;

	jmethodID mth_ids[mth_last];
};

static void jc_call_void(be_java_coal_t *c, int mth_index, ...)
{
	JNIEnv *jni   = c->env->jni;
	jmethodID mid = c->mth_ids[mth_index];

	va_list args;

	va_start(args, mth_index);
	(*jni)->CallVoidMethodV(jni, c->obj, mid, args);
	CHECK(c->env);
	va_end(args);
}

static int jc_call_int(be_java_coal_t *c, int mth_index, ...)
{
	JNIEnv *jni   = c->env->jni;
	jmethodID mid = c->mth_ids[mth_index];

	int res;
	va_list args;

	va_start(args, mth_index);
	res = (*jni)->CallIntMethodV(jni, c->obj, mid, args);
	CHECK(c->env);
	va_end(args);

	return res;
}

be_java_coal_t *be_java_coal_init(const char *graph_name, int n_nodes, int n_regs, int dbg_level)
{
	be_java_coal_t *c;
	jni_env_t *env = get_jvm();
	JNIEnv *jni = env->jni;
	jmethodID fact;
	jclass cls;
	jstring str;
	int i;

	c = malloc(sizeof(c[0]));
	memset(c, 0, sizeof(c[0]));
	c->env = env;

	/* Find the class we are are looking for. */
	cls = (*jni)->FindClass(jni, cls_name);
	CHECK(env);

	/* Get the static factory method. */
	fact = (*jni)->GetStaticMethodID(jni, cls, mthi_factory.name, mthi_factory.sig);
	CHECK(env);

	/* Call the factory. */
	str = (*jni)->NewStringUTF(jni, graph_name);
	CHECK(env);
	c->obj = (*jni)->CallStaticObjectMethod(jni, cls, fact, str, n_nodes, n_regs, dbg_level);
	CHECK(env);
	c->cls = (*jni)->GetObjectClass(jni, c->obj);

	/* Reference the created object. */
	c->obj = (*jni)->NewGlobalRef(jni, c->obj);
	CHECK(env);

	/* Lookup the member methods of the object. */
	for(i = 0; i < mth_last; ++i) {
		c->mth_ids[i] = (*jni)->GetMethodID(jni, c->cls, mthis[i].name, mthis[i].sig);
		CHECK(env);
	}

	return c;
}

void be_java_coal_destroy(be_java_coal_t *c) {
	JNIEnv *jni = c->env->jni;
	jc_call_void(c, mth_finish);
	(*jni)->DeleteGlobalRef(jni, c->obj);
	free(c);
}

void be_java_coal_add_int_edge(be_java_coal_t *c, int n, int m)
{
	jc_call_void(c, mth_add_int_edge, (jint) n, (jint) m);
}

void be_java_coal_add_aff_edge(be_java_coal_t *c, int n, int m, int weight)
{
	jc_call_void(c, mth_add_aff_edge, (jint) n, (jint) m, (jint) weight);
}

void be_java_coal_set_color(be_java_coal_t *c, int n, int col)
{
	jc_call_void(c, mth_set_color, (jint) n, (jint) col);
}

void be_java_coal_set_debug(be_java_coal_t *c, int n, const char *dbg)
{
#if 0
	JNIEnv *jni   = c->env->jni;
	jmethodID mid = c->mth_ids[mth_set_debug];
	jstring str;

	str = (*jni)->NewStringUTF(jni, dbg);
	CHECK(c->env);
	(*jni)->CallVoidMethod(jni, c->obj, mid, (jint) n, str);
	CHECK(c->env);
#endif
}

void be_java_coal_forbid_color(be_java_coal_t *c, int n, int col)
{
	jc_call_void(c, mth_forbid_color, (jint) n, (jint) col);
}

void be_java_coal_coalesce(be_java_coal_t *c)
{
	jc_call_void(c, mth_coalesce);
}

void be_java_coal_dump(be_java_coal_t *c, const char *fn)
{
	JNIEnv *jni   = c->env->jni;
	jmethodID mid = c->mth_ids[mth_dump];
	jstring str;

	str = (*jni)->NewStringUTF(jni, fn);
	CHECK(c->env);
	(*jni)->CallVoidMethod(jni, c->obj, mid, str);
	CHECK(c->env);
}

int be_java_coal_get_color(be_java_coal_t *c, int n)
{
	return jc_call_int(c, mth_get_color, (jint) n);
}

void be_java_coal_start_jvm(void)
{
	get_jvm();
}

#else

be_java_coal_t *be_java_coal_init(const char *graph_name, int n_nodes, int n_regs, int dbg_level)
{
	assert(0 && "use --enable-jvm");
	return NULL;
}

void be_java_coal_destroy(be_java_coal_t *c)
{
	assert(0 && "use --enable-jvm");
}


void be_java_coal_add_int_edge(be_java_coal_t *c, int n, int m)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_add_aff_edge(be_java_coal_t *c, int n, int m, int weight)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_set_color(be_java_coal_t *c, int n, int col)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_set_debug(be_java_coal_t *c, int n, const char *dbg)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_forbid_color(be_java_coal_t *c, int n, int col)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_coalesce(be_java_coal_t *c)
{
	assert(0 && "use --enable-jvm");
}

void be_java_coal_dump(be_java_coal_t *c, const char *fn)
{
	assert(0 && "use --enable-jvm");
}

int be_java_coal_get_color(be_java_coal_t *c, int n)
{
	assert(0 && "use --enable-jvm");
	return -1;
}

void be_java_coal_start_jvm(void)
{
}

#endif /* WITH_JVM */
