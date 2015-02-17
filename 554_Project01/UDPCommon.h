
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#define UDPCOMMON

#ifndef PROJECT1TYPES
	#define PROJECT1TYPES
	#include "Project1Types.h"
#endif

#define RECIPIENT_PORT 4636

void makeReceiverSA(struct sockaddr_in *sa, int port);
void makeDestSA(struct sockaddr_in * sa, char *hostname, int port);
void makeLocalSA(struct sockaddr_in *sa);
void printSA(struct sockaddr_in sa);
Status UDPreceive(int s, Message *m, SocketAddress *origin);
Status UDPsend(int s, Message *m, SocketAddress destination);
void getStatusString(char *dest, Status status);
