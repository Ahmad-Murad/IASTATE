all : EchoServer EchoClient

EchoServer : EchoServer.c  UDPCommon.c UDPCommon.h Project1Types.h
	gcc -o EchoServer EchoServer.c UDPCommon.c UDPCommon.h Project1Types.h
	
EchoClient : EchoClient.c  UDPCommon.c UDPCommon.h Project1Types.h
	gcc -o EchoClient EchoClient.c UDPCommon.c UDPCommon.h Project1Types.h

