/**
 * Some macros for wrapper function implementation.
 * @author Sebastian Hack
 * @date 9.12.2004
 */

#ifndef _IMPL_H
#define _IMPL_H

#define _HEAD1(name,res,t1) \
	res (name)(t1 p1)

#define _HEAD2(name,res,t1,t2) \
	res (name)(t1 p1, t2 p2)

#define _HEAD3(name,res,t1,t2,t3) \
	res (name)(t1 p1, t2 p2, t3 p3)

#define _HEAD4(name,res,t1,t2,t3,t4) \
	res (name)(t1 p1, t2 p2, t3 p3, t4 p4)


#define _IMPL1(name,prefix,ret,res,t1) \
_HEAD1(name, res, t1) { \
	ret prefix ## name (p1); \
}

#define _IMPL2(name,prefix,ret,res,t1,t2) \
_HEAD2(name, res, t1, t2) { \
	ret prefix ## name (p1, p2); \
}

#define _IMPL3(name,prefix,ret,res,t1,t2,t3) \
_HEAD3(name, res, t1, t2, t3) { \
	ret prefix ## name (p1, p2, p3); \
}

#define _IMPL4(name,prefix,ret,res,t1,t2,t3,t4) \
_HEAD4(name, res, t1, t2, t3, t4) { \
	ret prefix ## name (p1, p2, p3, p4); \
}

#define IMPL1_VOID(name,prefix,t1) \
  _IMPL1(name, prefix, (void), void, t1)

#define IMPL2_VOID(name,prefix,t1,t2) \
  _IMPL2(name, prefix, (void), void, t1, t2)

#define IMPL3_VOID(name,prefix,t1,t2,t3) \
  _IMPL3(name, prefix, (void), void, t1, t2, t3)

#define IMPL4_VOID(name,prefix,t1,t2,t3,t4) \
  _IMPL4(name, prefix, (void), void, t1, t2, t3, t4)


#define IMPL1(name,type,prefix,t1) \
  _IMPL1(name, prefix, return, type, t1)

#define IMPL2(name,type,prefix,t1,t2) \
  _IMPL2(name, prefix, return, type, t1, t2)

#define IMPL3(name,type,prefix,t1,t2,t3) \
  _IMPL3(name, prefix, return, type, t1, t2, t3)

#define IMPL4(name,type,prefix,t1,t2,t3,t4) \
  _IMPL4(name, prefix, return, type, t1, t2, t3, t4)


#define FIRM_IMPL1(name,type,t1) \
  _IMPL1(name, __, return, type, t1)

#define FIRM_IMPL2(name,type,t1,t2) \
  _IMPL2(name, __, return, type, t1, t2)

#define FIRM_IMPL3(name,type,t1,t2,t3) \
  _IMPL3(name, __, return, type, t1, t2, t3)

#define FIRM_IMPL4(name,type,t1,t2,t3,t4) \
  _IMPL4(name, __, return, type, t1, t2, t3, t4)


#define FIRM_IMPL1_VOID(name,t1) \
  _IMPL1(name, __, (void), void, t1)

#define FIRM_IMPL2_VOID(name,t1,t2) \
  _IMPL2(name, __, (void), void, t1, t2)

#define FIRM_IMPL3_VOID(name,t1,t2,t3) \
  _IMPL3(name, __, (void), void, t1, t2, t3)

#define FIRM_IMPL4_VOID(name,t1,t2,t3,t4) \
  _IMPL4(name, __, (void), void, t1, t2, t3, t4)

#define FIRM_PROTO1(name,type,t1) 						_HEAD1(name, type, t1)
#define FIRM_PROTO2(name,type,t1,t2) 					_HEAD2(name, type, t1, t2)
#define FIRM_PROTO3(name,type,t1,t2,t3) 			_HEAD3(name, type, t1, t2, t3)
#define FIRM_PROTO4(name,type,t1,t2,t3,t4) 		_HEAD4(name, type, t1, t2, t3, t4)

#endif
