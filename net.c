
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>

#include <net.h>

#include <scheme.h>

const int ipproto_tcp = IPPROTO_TCP;
const int ipproto_udp = IPPROTO_UDP;

int get_errno() {
	return errno;
}

ptr int_errno(int fd) {
	return Scons(Sinteger(fd), Sinteger(errno));
}

ptr c_dial(int proto, const char *host, const char *serv) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM,
		.ai_flags = AI_PASSIVE,
		.ai_protocol = proto
	};
	struct addrinfo *res = 0;
	int err;
	int sock;

	err = getaddrinfo(host, serv, 0, &res);
	if (err) fprintf(stderr, "Get Address Info Error: %s\n", gai_strerror(err));

	sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	err = errno;
	if (sock < 0) fprintf(stderr, "Socket Error: %s\n", strerror(err));
	err = connect(sock, res->ai_addr, res->ai_addrlen);
	if (err == -1) fprintf(stderr, "Connect Error: %s\n", strerror(errno));

	if (res) freeaddrinfo(res);
	return Scons(Sinteger(sock), Sinteger(err));
}

