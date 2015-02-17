#include "ArithmeticService.h"

Status add(int a, int b, int *result){
	if(a > INT_MAX - b)
		return Overflow;
	*result = a + b;
	return Ok;
}


Status sub(int a, int b, int *result){
	// don't need to worry about underflow because 
	// we are only using positive ints
	*result = a - b;
	return Ok;
}

Status mult(int a, int b, int *result){
	if(a > INT_MAX / b)
		return Overflow;
	*result = a*b;
	return Ok;
}

Status divide(int a, int b, int *result){
	if(b == 0)
		return DivZero;
	*result = a/b;
	return Ok;
}

Status dispatch(int procedureID, int a, int b, int *result){
	switch(procedureID){
		case ADD:
			return add(a, b, result);
		case SUB:
			return sub(a, b, result);
		case MULT:
			return mult(a, b, result);
		case DIV:
			return divide(a, b, result);
		default:
			printf("No such procedureID (%d)\n", procedureID);
			return Bad;
	}
}