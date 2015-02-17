#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "UDPCommon.h"

Status UDPreceive(int s, Message *m, SocketAddress *origin){
	struct sockaddr_in fromAddr;
	fromAddr.sin_family = AF_INET;
	int len, addrLen = sizeof(fromAddr);
	if((len = recvfrom(s, m->data,  SIZE, 0, (struct sockaddr *)&fromAddr, &addrLen)) < 0){
		perror("Error receiving message.") ;
		return Bad;
	} else {
		m->data[len] = 0;
		memcpy(origin, &fromAddr, sizeof(fromAddr));
		return Ok;
	}
}

Status UDPsend(int s, Message *m, SocketAddress destination){
	int n, destLen = sizeof(SocketAddress);
	if((n = sendto(s, m->data, m->length, 0, (const struct sockaddr *)&destination, destLen)) < 0){
		perror("Send failed");
		return Bad;
	}
	if(n != m->length) {
		printf("Sent %d chars, but should have sent %d.\n",n, m->length);
		return Wronglength;
	}
	return Ok;
}

/* make a socket address for a destination whose machine and port
	are given as arguments */
void makeDestSA(struct sockaddr_in * sa, char *hostname, int port)
{
	struct hostent *host;

	sa->sin_family = AF_INET;
	if((host = gethostbyname(hostname))== NULL){
		printf("Unknown host name: %s\n", hostname);
		exit(-1);
	}
	sa-> sin_addr = *(struct in_addr *) (host->h_addr);
	sa->sin_port = htons(port);
}

/* make a socket address using any of the addressses of this computer
for a local socket on any port */
void makeLocalSA(struct sockaddr_in *sa)
{
	sa->sin_family = AF_INET;
	sa->sin_port = htons(0);
	sa-> sin_addr.s_addr = htonl(INADDR_ANY);
}

/* make a socket address using any of the addressses of this computer
for a local socket on given port */
void makeReceiverSA(struct sockaddr_in *sa, int port)
{
	sa->sin_family = AF_INET;
	sa->sin_port = htons(port);
	sa->sin_addr.s_addr = htonl(INADDR_ANY);
}

void printSA(struct sockaddr_in sa)
{
	char mybuf[80];
	inet_ntop(AF_INET, &sa.sin_addr, mybuf, 80);
	printf("sa = %d, %s, %d\n", sa.sin_family, mybuf, ntohs(sa.sin_port));
}

void getStatusString(char *dest, Status status){
	if(status == Ok)
		sprintf(dest, "Ok");
	else if(status == Bad)
		sprintf(dest, "Bad");
	else
		sprintf(dest, "Wronglength");
}
