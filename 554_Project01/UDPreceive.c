#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>

struct hostent *gethostbyname() ;
void printSA(struct sockaddr_in sa) ;
void receiver() ;
void sender(char *message1, char *message2,char *machine, int port);

#define SIZE 1000

void main(int argc,char **argv)
{
	receiver();
}

/*print a socket address */
void printSA(struct sockaddr_in sa)
{
	char mybuf[80];
	char *ptr=inet_ntop(AF_INET, &sa.sin_addr, mybuf, 80);
	printf("sa = %d, %s, %d\n", sa.sin_family, mybuf, ntohs(sa.sin_port));
}

/* make a socket address using any of the addressses of this computer
for a local socket on given port */
void makeReceiverSA(struct sockaddr_in *sa, int port)
{
	sa->sin_family = AF_INET;
	sa->sin_port = htons(port);
	sa-> sin_addr.s_addr = htonl(INADDR_ANY);
}

/*receive two messages via s new socket,
	print out the messages received and close the socket
	bind to  any of the addresses of this computer
	using port given as argument */
void receiver()
{
	int aPort = IPPORT_RESERVED + 3612;
	char message1[SIZE] = "";
	struct sockaddr_in mySocketAddress, aSocketAddress;
	int s,aLength, n;
	int i;

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

	printSA(mySocketAddress);
	aLength = sizeof(aSocketAddress);
	aSocketAddress.sin_family = AF_INET;
	int messages;
	do{
		memset(message1, 0, sizeof(message1));
		printf("\nListening for messages...");
		if((n = recvfrom(s, message1,  SIZE, 0, (struct sockaddr *)&aSocketAddress, &aLength))<0)
			perror("Receive 1") ;
		else{
			printf("\n Received Message:(%s)length = %d \n", message1,n);
		}
	}while(strcmp("q", message1) != 0);
	close(s);
}
