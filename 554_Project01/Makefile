all : EchoServer EchoClient RPCServer RPCClient

ECHODEPS = UDPCommon.c UDPCommon.h Project1Types.h
RPCDEPS  = UDPCommon.h Project1Types.h RPCCommon.h ArithmeticService.h UDPCommon.c RPCCommon.c ArithmeticService.c

EchoServer : EchoServer.c  $(ECHODEPS)
	gcc -o EchoServer EchoServer.c $(ECHODEPS)
	
EchoClient : EchoClient.c  $(ECHODEPS)
	gcc -o EchoClient EchoClient.c $(ECHODEPS)

RPCServer : RPCServer.c  $(RPCDEPS)
	gcc -o RPCServer  RPCServer.c  $(RPCDEPS)

RPCClient : RPCClient.c  $(RPCDEPS)
	gcc -o RPCClient  RPCClient.c  $(RPCDEPS)
