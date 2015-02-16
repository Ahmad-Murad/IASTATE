#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "UDPCommon.h"

Status DoOperation (Message *message, Message *reply, int s, SocketAddress serverSA);
void   getMessage(Message *newMsg);

void main(int argc,char **argv)
{
	if(argc != 2){
		printf("Usage: <serverIP>\n");
		return;
	}

	int s, n;
	char *machine = argv[1], hostn[80];
	struct sockaddr_in clientAddr, serverAddr;
	gethostname(hostn, 80);

	if(( s = socket(AF_INET, SOCK_DGRAM, 0))<0) {
		perror("socket failed");
		return;
	}

	makeReceiverSA(&clientAddr, RECIPIENT_PORT);
	if( bind(s, (struct sockaddr *)&clientAddr, sizeof(struct sockaddr_in))!= 0){
		perror("Bind failed\n");
		close (s);
		return;
	}
	makeDestSA(&serverAddr, machine, RECIPIENT_PORT);
	printf("Connected to server: ");
	printSA(serverAddr);
	
	Message *msg = malloc(sizeof(Message)), *reply = malloc(sizeof(Message));
	do{
		getMessage(msg);
		DoOperation(msg, reply, s, serverAddr);
	}while(strcmp("q", msg->data) != 0);
	free(msg);
	free(reply);
	close(s);
}

void getMessage(Message *newMsg){
	printf("Message to send: ");
	fgets(newMsg->data, SIZE, stdin);
	newMsg->length = strlen(newMsg->data)-1; // -1 to remove the newline char
	newMsg->data[newMsg->length] = 0; // remove the newline
	return;
}

Status DoOperation (Message *message, Message *reply, int s, SocketAddress serverSA){
	int status = UDPsend(s, message, serverSA);
	if(Ok != status){
		printf("Send failed with code: %d\n", status);
		perror("Send failed");
		return status;
	}
	printf("Sent message: %s\n", message->data);
	status = UDPreceive(s, reply, &serverSA);
	if(Ok != status){
		perror("Listening for reply failed");
		return status;
	}
	printf("\tGot response (%s) from the server\n", reply->data);

	return Ok;
}
