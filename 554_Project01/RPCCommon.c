#include "RPCCommon.h"

// Pack an RPCMessage into a Message
void marshal(RPCMessage *rm, Message *message){
	message->length = sizeof(RPCMessage);

	int i;
	short *msgPtr = (short *)message->data;
	short *rmPtr = (short *)rm;
	for(i = 0; i*sizeof(short) < sizeof(RPCMessage); i++){
		msgPtr[i] = htons(rmPtr[i]);
		//printf("%4x --> %4x\n", rmPtr[i], msgPtr[i]);
	}
}

// Convert from Message to RPCMessage
void unMarshal(RPCMessage *rm, Message *message){
	int i;
	short *msgPtr = (short *)message->data;
	short *rmPtr = (short *)rm;
	for(i = 0; i*sizeof(short) < sizeof(RPCMessage); i++){
		rmPtr[i] = ntohs(msgPtr[i]);
		//printf("%4x <-- %4x\n", rmPtr[i], msgPtr[i]);
	}
}

void printRPCMessage(RPCMessage *rm){
	if(rm->messageType)
		printf("Request,");
	else
		printf("Reply,  ");

	switch(rm->procedureId){
		case STOP:
			printf("ProcID=STOP,"); 	break;
		case ADD:
			printf("ProcID=ADD, ");		break;
		case SUB:
			printf("ProcID=SUB, ");		break;
		case MULT:
			printf("ProcID=MULT,");		break;
		case DIV:
			printf("ProcID=DIV, ");		break;
		case PING:
			printf("ProcID=PING,");		break;
		default:
			printf("ProcID=???UNKNOWN???");
	}
	printf(" ID=%03u, arg1=%03d, arg2=%03d\n", rm->RPCId, rm->arg1, rm->arg2);
}
