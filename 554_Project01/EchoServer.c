#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "UDPCommon.h"

Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA);
Status SendReply(Message *replyMessage, int s, SocketAddress clientSA);

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
	do{
		Status stat = GetRequest(curMessage, s, (SocketAddress *)&clientAddr);
		printf("Got message: %s\n", curMessage->data);

		getStatusString(reply->data, stat);
		reply->length = strlen(reply->data);
	
		Status replyStatus = SendReply(reply, s, clientAddr);
		printf("\t Sent reply (%s) with status %d\n", reply->data, replyStatus);
	} while (strcmp("q", curMessage->data) != 0);
	free(curMessage);
	free(reply);

	close(s);
}

Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA){
	return UDPreceive(s, callMessage, clientSA);
}

Status SendReply(Message *replyMessage, int s, SocketAddress clientSA){
	return UDPsend(s, replyMessage, clientSA);
}
