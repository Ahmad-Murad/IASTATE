#include <sys/time.h>
#include "RPCCommon.h"

unsigned int nextRPCId = 1;

Status DoOperation (Message *message, Message *reply, int s, SocketAddress serverSA);
void   getMessage(RPCMessage *newMsg);
Status waitForReply(int s, int seconds, int nanos);

void main(int argc,char **argv)
{
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
		printf("\tSending RPC msg: ");
		printRPCMessage(rpcMsg);
		marshal(rpcMsg, msg);
		Status opStatus = DoOperation(msg, reply, s, serverAddr);
		unMarshal(rpcReply, reply);
		if(opStatus == Ok)
			printf("\tResult of message %03d is: %d\n", rpcReply->RPCId, rpcReply->arg1);
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
	int status, maxRetries = 5, retries = 0;

	do {
		status = UDPsend(s, message, serverSA);
		if (Ok != status){
			printf("Send failed with code: %d\n", status);
			perror("Send failed");
			return status;
		}
		status = waitForReply(s, 3, 0);
		retries++;
		if(status == TimedOut && retries <= maxRetries)
			printf("\tSending message timed out -- resending message.\n");
		if(retries > maxRetries){
			printf("\tMax retries have been reached -- aborting.\n");
			return TimedOut;
		}
	} while (status != Ok);

	status = UDPreceive(s, reply, &serverSA);
	if(Ok != status){
		perror("Listening for reply failed");
		return status;
	}
	return Ok;
}

Status waitForReply(int s, int seconds, int nanos)
{
	unsigned long read_mask = (1<<s);
	struct timeval timeout;
	int n, maxRetries = 3, numRetries = 0;

	timeout.tv_sec  = seconds; 
	timeout.tv_usec = nanos; 

	if((n = select(32, (fd_set *)&read_mask, 0, 0, &timeout)) < 0)
		perror("Select failed");

	return (n == 1) ? Ok : TimedOut;
}
