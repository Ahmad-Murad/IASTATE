#include "RPCCommon.h"

unsigned int nextRPCId = 1;

Status DoOperation (Message *message, Message *reply, int s, SocketAddress serverSA);
void   getMessage(RPCMessage *newMsg);

void main(int argc,char **argv)
{
	// use to debug marshal and unmarshalling
	if(1 != 1){
		RPCMessage *rm = malloc(sizeof(RPCMessage));
		memset(rm, '?', sizeof(RPCMessage));
		rm->messageType = Reply;
		rm->RPCId = 12345;
		rm->procedureId = 3;
		rm->arg1 = 2222;
		rm->arg2 = 3333;
		Message *mes  = malloc(sizeof(Message));
		memset(mes, '?', sizeof(Message));
		marshal(rm, mes);
		RPCMessage *result = malloc(sizeof(RPCMessage));
		memset(result, 0, 20);
		unMarshal(result, mes);
		printf("type: %d\n",result->messageType);
		printf("PRCID: %d\n",result->RPCId);
		printf("procid: %d\n",result->procedureId);
		printf("arg1: %d\n",result->arg1);
		printf("arg2: %d\n",result->arg2);
		return;
	}


	if(argc != 2){
		printf("Usage: <serverIP>\n");
		return;
	}

	int s, n;
	char *machine = argv[1];
	struct sockaddr_in clientAddr, serverAddr;

	if(( s = socket(AF_INET, SOCK_DGRAM, 0))<0) {
		perror("socket failed");
		return;
	}

	makeLocalSA(&clientAddr);
	if( bind(s, (struct sockaddr *)&clientAddr, sizeof(struct sockaddr_in))!= 0){
		perror("Bind failed\n");
		close (s);
		return;
	}
	makeDestSA(&serverAddr, machine, RECIPIENT_PORT);
	printf("Connected to server: ");
	printSA(serverAddr);
	
	Message *msg = malloc(sizeof(Message)), *reply = malloc(sizeof(Message));
	RPCMessage *rpcMsg = malloc(sizeof(RPCMessage)), *rpcReply = malloc(sizeof(RPCMessage));
	do{
		getMessage(rpcMsg);
		printf("Sending RPC msg: ");
		printRPCMessage(rpcMsg);
		marshal(rpcMsg, msg);
		DoOperation(msg, reply, s, serverAddr);
		unMarshal(rpcReply, reply);
		printf("\tGot result: %03d from the server\n", rpcReply->arg1);
	}while(rpcMsg->procedureId != STOP);
	free(rpcMsg);
	free(rpcReply);
	free(msg);
	free(reply);
	close(s);
}

char *nextInt(char *buf, int *next){
	int ret = 0;
	while(*buf == ' ') buf++; // skip whitespace
	while(*buf >= '0' && *buf <= '9'){
		ret *= 10;
		ret += (*buf - '0');
		buf++;
	}
	*next = ret;
	return buf;
}

char *nextOp(char *buf, unsigned int *next){
	while(*buf == ' ') buf++; // skip whitespace
	if(*buf == '+')
		*next = ADD;
	else if(*buf == '-')
		*next = SUB;
	else if(*buf == '*')
		*next = MULT;
	else if(*buf == '/')
		*next = DIV;
	return ++buf;
}

void getMessage(RPCMessage *newMsg){
	printf("Enter an Message: ");
	char input[80], *curPtr = input;
	fgets(input, 80, stdin);
	input[strlen(input)-1] = 0; // remove newline char
	newMsg->messageType = Request;
	newMsg->RPCId       = nextRPCId++;
	if(strcasecmp("Stop", input) == 0){
		newMsg->procedureId = STOP;
	} else if(strcasecmp("Ping", input) == 0){
		newMsg->procedureId = PING;
	} else {
		curPtr = nextInt(curPtr, &(newMsg->arg1));
		curPtr = nextOp(curPtr, &(newMsg->procedureId));
		curPtr = nextInt(curPtr, &(newMsg->arg2));
	}
}

Status DoOperation (Message *message, Message *reply, int s, SocketAddress serverSA){
	int status = UDPsend(s, message, serverSA);
	if(Ok != status){
		printf("Send failed with code: %d\n", status);
		perror("Send failed");
		return status;
	}
	status = UDPreceive(s, reply, &serverSA);
	if(Ok != status){
		perror("Listening for reply failed");
		return status;
	}
	return Ok;
}
