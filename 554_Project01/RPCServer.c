#include "RPCCommon.h"
#include "ArithmeticService.h"

Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA);
Status SendReply(Message *replyMessage, int s, SocketAddress clientSA);
Status processRequest(RPCMessage *request, RPCMessage *response);

void main(int argc,char **argv)
{
	int aPort = IPPORT_RESERVED + 3612;
	struct sockaddr_in serverAddr, clientAddr;
	int s, n, i;

	if((s = socket(AF_INET, SOCK_DGRAM, 0))<0) {
		perror("socket failed");
		return;
	}
	makeReceiverSA(&serverAddr, aPort);
	if( bind(s, (struct sockaddr *)&serverAddr, sizeof(struct sockaddr_in))!= 0){
		perror("Bind failed\n");
		close(s);
		return;
	}
	
	printf("Listening for messages...\n");
	Message *curMessage = malloc(sizeof(Message)), *reply = malloc(sizeof(Message));
	RPCMessage *curRPC = malloc(sizeof(RPCMessage)), *replyRPC = malloc(sizeof(RPCMessage));
	do{
		Status stat = GetRequest(curMessage, s, (SocketAddress *)&clientAddr);

		unMarshal(curRPC, curMessage);
		printf("Got RPC Message: ");
		printRPCMessage(curRPC);

		stat = processRequest(curRPC, replyRPC);
		printf("Created reply:   ");
		printRPCMessage(replyRPC);

		marshal(replyRPC, reply);
		Status replyStatus = SendReply(reply, s, clientAddr);
		printf("\tSent reply with status %d\n", replyStatus);
	} while (curRPC->procedureId != STOP);
	free(curRPC);
	free(curMessage);
	free(reply);

	close(s);
}

Status processRequest(RPCMessage *request, RPCMessage *response){
	int result = 0;
	Status opStatus;
	memset(response, 0, sizeof(RPCMessage));
	response->messageType = Reply;
	response->RPCId = request->RPCId;
	response->procedureId = request->procedureId;

	switch(request->procedureId){
		case STOP:
			return Ok;
		case PING:
			return Ok;
		default:
			opStatus = dispatch(request->procedureId, request->arg1, request->arg2, &result);
			response->arg1 = result;
			response->arg2 = 0;
			return opStatus;
	}
}

Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA){
	return UDPreceive(s, callMessage, clientSA);
}

Status SendReply(Message *replyMessage, int s, SocketAddress clientSA){
	return UDPsend(s, replyMessage, clientSA);
}
