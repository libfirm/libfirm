#include <stdio.h>
#include "define_operation.h"

typedef int sse_register;

union input_register_set
{
	sse_register hardware_reg_1;
	sse_register hardware_reg_2;
	sse_register hardware_reg_3;
} input_register_set;

union output_register_set
{
	sse_register hardware_reg_1;
	sse_register hardware_reg_2;
	sse_register hardware_reg_3;
} output_register_set;

int memory_array[100];

typedef int sse_32_2;


void vadd_memory(void)
{
	// Only Register
	/*sse_32_2 *param0 = SSEArg_0(input_register_set);
	sse_32_2 *param1 = SSEArg_1(input_register_set);
	sse_32_2 *result = SSEArg_Res(output_register_set); */

	// Only Memory
	/*sse_32_2 *param0 = SSEArg_0(memory_array);
	sse_32_2 *param1 = SSEArg_1(memory_array);
	sse_32_2 *result = SSEArg_Res(memory_array); */

	// Mixed
	sse_32_2 *param0 = Arg_0(memory_array);
	sse_32_2 *param1 = Arg_1(memory_array);
	sse_32_2 *result = Res(&output_register_set);

	result[0] = param0[0] + param1[0];
	result[1] = param0[0] + param1[1];

/*	int a0, a1, b0, b1, r0, r1;

	a0 = param0[0];
	a1 = param0[1];
	b0 = param1[0];
	b1 = param1[1];

	if(a0 > b1)
	{
		r0 = a0 + a0 + b0; //result[0] = param0[0] + param0[0] + param1[0];
		r1 = a1 + a0 + b1; //result[1] = param0[1] + param0[0] + param1[1];
	}
	else
	{
		r0 = a0; //result[0] = param0[0];
		r1 = a1; //result[1] = param0[1];
	}

	result[0] = r0;
	result[1] = r1; */
}

 void vstore(void)
{
	sse_32_2 *param = Arg_0(&input_register_set);
	sse_32_2 *result = Arg_1(memory_array);

	result[0] = param[0];
	result[1] = param[1];
}
