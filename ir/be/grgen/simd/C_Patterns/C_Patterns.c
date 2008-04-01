/*****************************************************************************
 * Program:  C_Patterns.c
 * Function: C specification of complex operations for the simd optimization.
 *			 The order in this file of the functions is IMPORTANT since an
 *			 earlier defined function has a higher scheduling priority!
 * Author:   Andreas Schoesser
 * Date:     2007-02-13
 *****************************************************************************/


#include <stdio.h>
#include "define_operation.h"

typedef int sse_32_2;

/* Vektor Load, 4 Komponents, single precision float */
void vload_4f_32(void)
{
	float *param = Arg_0("vector", "memory", "gp");
	float *result = Res("vector", "register", "xmm");
	Emit(". movaps (%S0), %D0");
	Priority(0);

	result[0] = param[0];
	result[1] = param[1];
	result[2] = param[2];
	result[3] = param[3];
}



void vload_16b(void)
{
	unsigned char *param  = Arg_0("vector", "memory", "gp");
	unsigned char *result = Res("vector", "register", "xmm");
	Emit(". lddqu (%S0), %D0");
	Priority(0);
	CostSavings(10);

	result[0] = param[0];
	result[1] = param[1];
	result[2] = param[2];
	result[3] = param[3];
	result[4] = param[4];
	result[5] = param[5];
	result[6] = param[6];
	result[7] = param[7];
	result[8] = param[8];
	result[9] = param[9];
	result[10] = param[10];
	result[11] = param[11];
	result[12] = param[12];
	result[13] = param[13];
	result[14] = param[14];
	result[15] = param[15];
}



#if 0
/* Vektor Load, 4 Komponents, 32 Bit integer */
void vload_4_32(void)
{
	sse_32_2 *param = Arg_0("vector", "memory", "gp");
	sse_32_2 *result = Res("vector", "register", "xmm");
	Emit(". movdqu (%S0), %D0");
	Priority(0);

	result[0] = param[0];
	result[1] = param[1];
	result[2] = param[2];
	result[3] = param[3];
}


void vload_2_32(void)
{
	sse_32_2 *param = Arg_0("vector", "memory", "gp");
	sse_32_2 *result = Res("vector", "register", "xmm");
	Emit(". movq (%S0), %D0");
	Priority(1);

	result[0] = param[0];
	result[1] = param[1];
}


void vadd_2_32(void)
{
	sse_32_2 *param0 = Arg_0("vector", "register", "xmm");
	sse_32_2 *param1 = Arg_1("vector", "register", "xmm");
	sse_32_2 *result = Res("vector", "register", "in_r0");
	Emit(". paddd %S1, %S0");
	Priority(2);


	result[0] = param0[0] + param1[0];
	result[1] = param0[1] + param1[1];
}
#endif

/** Register mode **/
void mulps_4_32(void)
{
	float *param0 = Arg_0("vector", "register", "xmm");
	float *param1 = Arg_1("vector", "register", "xmm");
	float *result = Res("vector", "register", "in_r1");
	Emit(". mulps %S1, %S0");
	Priority(2);

	result[0] = param0[0] * param1[0];
	result[1] = param0[1] * param1[1];
	result[2] = param0[2] * param1[2];
	result[3] = param0[3] * param1[3];
}

/** Mem mode right **/
void mulps_4_32_am(void)
{
	float *param0 = Arg_0("vector", "register", "xmm");
	float *param1 = Arg_1("vector", "memory",   "gp");
	float *result = Res("vector", "register", "in_r1");
	Emit(". mulps %S1, %S0");
	Priority(0);
	CostSavings(5);

	result[0] = param0[0] * param1[0];
	result[1] = param0[1] * param1[1];
	result[2] = param0[2] * param1[2];
	result[3] = param0[3] * param1[3];
}



void add_horz_4_32(void)
{
	float *param = Arg_0("vector", "register", "xmm");
	float *result = Res("vector", "register", "in_r1");
	Emit(". haddps %S0, %S0\\n. haddps %S0, %S0");
	Priority(2);

	result[0] = param[0] + param[1] + param[2] + param[3];
}


 /************************************************************************/
 /*                                                                      */
 /************************************************************************/

void maxps(void)
{
	float *a = Arg_0("vector", "register", "xmm");
	float *b = Arg_1("vector", "register", "xmm");
	float *r =   Res("vector", "register", "in_r1");
	Emit(". maxps %S1, %S0");
	Priority(2);

	if(a[0] < b[0])
		r[0] = b[0];
	else
		r[0] = a[0];

	if(a[1] < b[1])
		r[1] = b[1];
	else
		r[1] = a[1];

	if(a[2] < b[2])
		r[2] = b[2];
	else
		r[2] = a[2];

	if(a[3] < b[3])
		r[3] = b[3];
	else
		r[3] = a[3];
}

void psadbw()
{
	unsigned char *a = Arg_0("vector", "register", "xmm");
	unsigned char *b = Arg_1("vector", "register", "xmm");
	unsigned int  *r =   Res("vector", "register", "in_r1");
	Emit(". psadbw %S1, %S0\\n. phaddd %S0, %S0\\n. phaddd %S0, %S0");
	Priority(2);

	r[0] = ((a[0] > b[0]) ? (a[0] - b[0]) : (b[0] - a[0])) +
	       ((a[1] > b[1]) ? (a[1] - b[1]) : (b[1] - a[1])) +
	       ((a[2] > b[2]) ? (a[2] - b[2]) : (b[2] - a[2])) +
		   ((a[3] > b[3]) ? (a[3] - b[3]) : (b[3] - a[3])) +
		   ((a[4] > b[4]) ? (a[4] - b[4]) : (b[4] - a[4])) +
	       ((a[5] > b[5]) ? (a[5] - b[5]) : (b[5] - a[5])) +
	       ((a[6] > b[6]) ? (a[6] - b[6]) : (b[6] - a[6])) +
	       ((a[7] > b[7]) ? (a[7] - b[7]) : (b[7] - a[7])) +
	       ((a[8] > b[8]) ? (a[8] - b[8]) : (b[8] - a[8])) +
	       ((a[9] > b[9]) ? (a[9] - b[9]) : (b[9] - a[9])) +
	       ((a[10] > b[10]) ? (a[10] - b[10]) : (b[10] - a[10])) +
	       ((a[11] > b[11]) ? (a[11] - b[11]) : (b[11] - a[11])) +
	       ((a[12] > b[12]) ? (a[12] - b[12]) : (b[12] - a[12])) +
	       ((a[13] > b[13]) ? (a[13] - b[13]) : (b[13] - a[13])) +
	       ((a[14] > b[14]) ? (a[14] - b[14]) : (b[14] - a[14])) +
	       ((a[15] > b[15]) ? (a[15] - b[15]) : (b[15] - a[15]));
}

/*void sadps(void)
{
	float *a = Arg_0("vector", "register", "xmm");
	float *b = Arg_1("vector", "register", "xmm");
	float *r =   Res("vector", "register", "in_r1");

	r[0] = (a[0] - b[0]) + (a[1] - b[1]) + (a[2] - b[2]) + (a[3] - b[3]);


} */


void vstore_4f(void)
{
	float *param =   Arg_0("vector", "register", "xmm");
	float *result =  Arg_1("vector", "memory", "gp");
	Emit(". movaps %S0, (%S1)");
	Priority(4);

	result[0] = param[0];
	result[1] = param[1];
	result[2] = param[2];
	result[3] = param[3];
}



#if 0
/************************************************************************/
/* vstore                                                               */
/* Input:     Vector register 1 v1, memory pointer p1                   */
/* Output:    None.                                                     */
/* Operation: Store the components of v1 at memory location p1.         */
/************************************************************************/

void vstore_4_32(void)
{
	sse_32_2 *param = Arg_0("vector", "register", "xmm");
	sse_32_2 *result = Arg_1("vector", "memory", "gp");
	Emit(". movq %S0, (%S1)");
	Priority(4);

	result[0] = param[0];
	result[1] = param[1];
	result[2] = param[2];
	result[3] = param[3];
}


void vstore_2_32(void)
{
	sse_32_2 *param = Arg_0("vector", "register", "xmm");
	sse_32_2 *result = Arg_1("vector", "memory", "gp");
	Emit(". movq %S0, (%S1)");
	Priority(5);

	result[0] = param[0];
	result[1] = param[1];
}

#endif

void component_0f(void)
{
	float *b = Arg_0("vector", "register", "xmm");
	float *r = Res("scalar", "register", "xmm");
	Priority(PRIORITY_CLEANUP);
	Emit("");//. movd %S0, %D0");

	*r = b[0];
}




void component_0Iu(void)
{
	int *b = Arg_0("vector", "register", "xmm");
	int *r = Res("scalar", "register", "gp");
	Priority(PRIORITY_CLEANUP);
	Emit(". movd %S0, %D0");

	*r = b[0];
}
#if 0
void component_0(void)
{
	sse_32_2 *b = Arg_0("vector", "register", "xmm");
	sse_32_2 *r = Res("scalar", "register", "gp");
	Priority(PRIORITY_CLEANUP);
	Emit(". movd %S0, %D0");

	*r = b[0];
}

void component_1(void)
{
	sse_32_2 *b = Arg_0("vector", "register", "xmm");
	sse_32_2 *r = Res("scalar", "register", "gp");
	Destroys("in_r0");
	Priority(PRIORITY_CLEANUP);
	Emit(". psrldq \\$4, %S0 \\n. movd %S0, %D0");

	*r = b[1];
}

void component_2(void)
{
	sse_32_2 *b = Arg_0("vector", "register", "xmm");
	sse_32_2 *r = Res("scalar", "register", "gp");
	Destroys("in_r0");
	Priority(PRIORITY_CLEANUP);
	Emit(". psrldq \\$8, %S0 \\n. movd %S0, %D0");

	*r = b[2];
}

void component_3(void)
{
	sse_32_2 *b = Arg_0("vector", "register", "xmm");
	sse_32_2 *r = Res("scalar", "register", "gp");
	Destroys("in_r0");
	Priority(PRIORITY_CLEANUP);
	Emit(". psrldq \\$12, %S0 \\n. movd %S0, %D0");

	*r = b[3];
}


/********************************
 * THIS IS SSE3!
 ********************************/


void packed_add_8_32(void)
{
	sse_32_2 *a = Arg_0("vector", "register", "xmm");
	sse_32_2 *b = Arg_1("vector", "register", "xmm");
	sse_32_2 *r = Res("vector",   "register", "in_r0");
	Priority(3);
	Emit(". haddps %S1, %S0");

	r[0] = a[0] + a[1];
	r[1] = a[2] + b[3];
	r[2] = b[0] + b[1];
	r[3] = b[2] + b[3];
}
#endif
