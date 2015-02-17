#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#define RPCCOMMON
#ifndef UDPCOMMON
	#define UDPCOMMON
	#include "UDPCommon.h"
#endif
#ifndef ARITHMETICSERVICE
	#define ARITHMETICSERVICE
	#include "ArithmeticService.h"
#endif

/* Define procedure IDs    */
#define STOP 0
#define ADD  1
#define SUB  2
#define MULT 3
#define DIV  4
#define PING 5

typedef struct {
 enum {Request, Reply} messageType;  /* same size as an unsigned int */
 unsigned int RPCId;       /* unique identifier */
 unsigned int procedureId; /* e.g.(1,2,3,4) for (+, -, *, /) */
 int arg1;                 /* argument/ return parameter */
 int arg2;                 /* argument/ return parameter */
} RPCMessage;              /* each int (and unsigned int) is 32 bits = 4 bytes */

void marshal(RPCMessage *rm, Message *message);
void unMarshal(RPCMessage *rm, Message *message);
void printRPCMessage(RPCMessage *rm);