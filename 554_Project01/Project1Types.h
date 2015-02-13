#include <sys/types.h>
#include <sys/socket.h>

#define SIZE 1000
typedef struct {
	unsigned int length;
	unsigned char data[SIZE];
} Message;
typedef enum {
	Ok,          /* operation successful */
	Bad,         /* unrecoverable error  */
	Wronglength  /* bad message length   */
}Status;
 typedef struct sockaddr_in SocketAddress ;