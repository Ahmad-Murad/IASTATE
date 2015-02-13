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
void makeDestSA(struct sockaddr_in * sa, char *hostname, int port) ;
void makeLocalSA(struct sockaddr_in *sa) ;
void sender(char *machine);

#define RECIPIENT_PORT 4636
#define SIZE 1000

/* main for sender and receiver - to send give s machine messag1 and message2
	- to receive give r
	*/
void main(int argc,char **argv)
{
	if(argc <= 1){
		printf("Usage: <servername>\n");
		exit(1);
	}
	sender(argv[1]);
}

/*print a socket address */
void printSA(struct sockaddr_in sa)
{
	char mybuf[80];
	char *ptr=inet_ntop(AF_INET, &sa.sin_addr, mybuf, 80);
	printf("sa = %d, %s, %d\n", sa.sin_family, mybuf, ntohs(sa.sin_port));
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

void sender(char *machine)
{
	int s, n;
	struct sockaddr_in mySocketAddress, yourSocketAddress;

	if(( s = socket(AF_INET, SOCK_DGRAM, 0))<0) {
		perror("socket failed");
		return;
	}
/*
	 if((x = setsockopt(s, SOL_SOCKET, SO_BROADCAST, &arg, sizeof(arg))<0)
		perror("setsockopt SO_BROADCAST---");
		exit(-1);
  */
	makeLocalSA(&mySocketAddress);
	if( bind(s, (struct sockaddr *)&mySocketAddress, sizeof(struct sockaddr_in))!= 0){
		perror("Bind failed\n");
		close (s);
		return;
	}
	printSA(mySocketAddress);
	makeDestSA(&yourSocketAddress,machine, RECIPIENT_PORT);
	printSA(yourSocketAddress);
	
	char message[SIZE];
	do{
		fgets(message, SIZE, stdin);
		message[strlen(message)-1] = 0; // remove the newline
		printf("Sending %s\n", message);
		if( (n = sendto(s, message, strlen(message), 0, (struct sockaddr *)&yourSocketAddress,
			sizeof(struct sockaddr_in))) < 0)
			perror("Send failed\n");
	}while(strcmp("q", message) != 0);
	close(s);
}
