#include <limits.h>
#define ARITHMETICSERVICE
#ifndef PROJECT1TYPES
	#define PROJECT1TYPES
	#include "Project1Types.h"
#endif
#ifndef RPCCOMMON
	#define RPCCOMMON
	#include "RPCCommon.h"
#endif

Status dispatch(int procedureID, int a, int b, int *result);

Status add(int a, int b, int *result);

Status sub(int a, int b, int *result);

Status mult(int a, int b, int *result);

Status divide(int a, int b, int *result);