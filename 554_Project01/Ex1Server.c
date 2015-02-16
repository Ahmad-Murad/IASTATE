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
	struct sockaddr_in mySocketAddress, aSocketAddress;
	int s, n, i;

	if((s = socket(AF_INET, SOCK_DGRAM, 0))<0) {
		perror("socket failed");
		return;
	}
	makeReceiverSA(&mySocketAddress, aPort);

	if( bind(s, (struct sockaddr *)&mySocketAddress, sizeof(struct sockaddr_in))!= 0){
		perror("Bind failed\n");
		close(s);
		return;
	}
	printf("Connected to:  ");
	printSA(mySocketAddress);
	aSocketAddress.sin_family = AF_INET;
	
	printf("Listening for messages...\n");
	Message *curMessage = malloc(sizeof(*curMessage)), *reply = malloc(sizeof(*reply));
	do{
		Status stat = GetRequest(curMessage, s, (SocketAddress *)&aSocketAddress);
		printf("Got message: %s\n", curMessage->data);

		if(stat == Ok)
			sprintf(reply->data, "Ok");
		else if(stat == Bad)
			sprintf(reply->data, "Bad");
		else 
			sprintf(reply->data, "Wronglength");
		reply->length = strlen(reply->data);
		char addrBuff[80];
		inet_ntop(AF_INET, &aSocketAddress.sin_addr, addrBuff, 80);
		Status replyStatus = SendReply(reply, s, aSocketAddress);
		printf("\tSent reply (%s) with status: %d\n", reply->data, replyStatus);
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