/**
 * Some macros for facade function implementation.
 * @author Sebastian Hack
 * @date 9.12.2004
 */

#ifndef _IMPL_H
#define _IMPL_H

#define _IMPL1(name,prefix,ret,res,t1,p1) \
res name (t1 p1) { \
	ret prefix ## name (p1); \
}

#define _IMPL2(name,prefix,ret,res,t1,p1,t2,p2) \
res name (t1 p1, t2 p2) { \
	ret prefix ## name (p1, p2); \
}

#define _IMPL3(name,prefix,ret,res,t1,p1,t2,p2,t3,p3) \
res name (t1 p1, t2 p2, t3 p3) { \
	ret prefix ## name (p1, p2, p3); \
}

#define _IMPL4(name,prefix,ret,res,t1,p1,t2,p2,t3,p3,t4,p4) \
res name (t1 p1, t2 p2, t3 p3, t4 p4) { \
	ret prefix ## name (p1, p2, p3, p4); \
}

#define IMPL1_VOID(name,prefix,t1,p1) \
  _IMPL1(name, prefix, , void, t1, p1)

#define IMPL2_VOID(name,prefix,t1,p1,t2,p2) \
  _IMPL2(name, prefix, , void, t1, p1, t2, p2)

#define IMPL3_VOID(name,prefix,t1,p1,t2,p2,t3,p3) \
  _IMPL3(name, prefix, , void, t1, p1, t2, p2, t3, p3)

#define IMPL4_VOID(name,prefix,t1,p1,t2,p2,t3,p3,t4,p4) \
  _IMPL4(name, prefix, , void, t1, p1, t2, p2, t3, p3, t4, p4)


#define IMPL1(name,type,prefix,t1,p1) \
  _IMPL1(name, prefix, return, type, t1, p1)

#define IMPL2(name,type,prefix,t1,p1,t2,p2) \
  _IMPL2(name, prefix, return, type, t1, p1, t2, p2)

#define IMPL3(name,type,prefix,t1,p1,t2,p2,t3,p3) \
  _IMPL3(name, prefix, return, type, t1, p1, t2, p2, t3, p3)

#define IMPL4(name,type,prefix,t1,p1,t2,p2,t3,p3,t4,p4) \
  _IMPL4(name, prefix, return, type, t1, p1, t2, p2, t3, p3, t4, p4)


#define FIRM_IMPL1(name,type,t1,p1) \
  _IMPL1(name, __, return, type, t1, p1)

#define FIRM_IMPL2(name,type,t1,p1,t2,p2) \
  _IMPL2(name, __, return, type, t1, p1, t2, p2)

#define FIRM_IMPL3(name,type,t1,p1,t2,p2,t3,p3) \
  _IMPL3(name, __, return, type, t1, p1, t2, p2, t3, p3)

#define FIRM_IMPL4(name,type,t1,p1,t2,p2,t3,p3,t4,p4) \
  _IMPL4(name, __, return, type, t1, p1, t2, p2, t3, p3, t4, p4)


#define FIRM_IMPL1_VOID(name,t1,p1) \
  _IMPL1(name, __, , void, t1, p1)

#define FIRM_IMPL2_VOID(name,t1,p1,t2,p2) \
  _IMPL2(name, __, , void, t1, p1, t2, p2)

#define FIRM_IMPL3_VOID(name,t1,p1,t2,p2,t3,p3) \
  _IMPL3(name, __, , void, t1, p1, t2, p2, t3, p3)

#define FIRM_IMPL4_VOID(name,t1,p1,t2,p2,t3,p3,t4,p4) \
  _IMPL4(name, __, , void, t1, p1, t2, p2, t3, p3, t4, p4)

#endif
