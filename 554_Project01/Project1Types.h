#include <sys/types.h>
#include <sys/socket.h>
#define PROJECT1TYPES

#define SIZE 1000
typedef struct {
	unsigned int length;
	unsigned char data[SIZE];
} Message;
typedef enum Status {
	Ok,          /* operation successful */
	Bad,         /* unrecoverable error  */
	Wronglength, /* bad message length   */
	DivZero,
	Overflow
}Status;
 typedef struct sockaddr_in SocketAddress ;