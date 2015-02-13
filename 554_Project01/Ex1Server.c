#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "Project1Types.h"

Status UDPreceive(int s, Message *m, SocketAddress *origin);
Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA);
Status SendReply(Message *replyMessage, int s, SocketAddress clientSA);

void main(int argc,char **argv)
{
	int aPort = IPPORT_RESERVED + 3612;
	struct sockaddr_in mySocketAddress, aSocketAddress;
	int s,aLength, n, i;

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
	aLength = sizeof(aSocketAddress);
	aSocketAddress.sin_family = AF_INET;
	
	printf("Listening for messages...\n");
	Message *curMessage;
	do{
		Status stat = UDPreceive(s, curMessage, (SocketAddress *)&aSocketAddress);
		printf("Got message: %s\n", curMessage->data);
		printf("Status is: %d\n", stat);
	} while (strcmp("q", curMessage->data) != 0);

	close(s);
}

Status UDPreceive(int s, Message *m, SocketAddress *origin){

}

Status GetRequest(Message *callMessage, int s, SocketAddress *clientSA){
	memset(callMessage, 0, sizeof(callMessage));
	int len;
	if((len = recvfrom(s, callMessage,  SIZE, 0, (struct sockaddr *)&clientSA, &callMessage))<0){
		perror("Error receiving message.") ;
		return Status.Bad;
	} else{
		printf("\n Received Message:(%s)length = %d \n", callMessage,len);
		return Status.Ok;
	}
}

Status SendReply(Message *replyMessage, int s, SocketAddress clientSA){
	printf("Sending reply...\n");
	return Status.Ok;
}
