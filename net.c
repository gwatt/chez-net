
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <unistd.h>

#ifdef _WIN32
#  if __cplusplus
#    ifdef SCHEME_IMPORT
#      define EXPORT extern "C" __declspec (dllimport)
#    elif SCHEME_STATIC
#      define EXPORT extern "C"
#    else
#      define EXPORT extern "C" __declspec (dllexport)
#    endif
#  else
#    ifdef SCHEME_IMPORT
#      define EXPORT extern __declspec (dllimport)
#    elif SCHEME_STATIC
#      define EXPORT extern
#    else
#      define EXPORT extern __declspec (dllexport)
#    endif
#  endif
#else
#  if __cplusplus
#    define EXPORT extern "C"
#  else
#    define EXPORT extern
#  endif
#endif

struct fd_result {
	int fd;
	int errval;
	const char *(*strerror)(int);
};

EXPORT struct fd_result *c_dial(int proto, const char *host, const char *serv, struct fd_result *fdr);

const int ipproto_tcp = IPPROTO_TCP;
const int ipproto_udp = IPPROTO_UDP;

struct fd_result *c_dial(int proto, const char *host, const char *serv, struct fd_result *fdr) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM,
		.ai_flags = AI_PASSIVE,
		.ai_protocol = proto
	};
	struct addrinfo *addr = 0;

	fdr->errval = getaddrinfo(host, serv, 0, &addr);
	if (fdr->errval) {
		fdr->fd = -1;
		fdr->strerror = gai_strerror;
		goto out;
	}

	fdr->fd = socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
	if (fdr->fd < 0) {
		fdr->strerror = strerror;
		fdr->errval = errno;
		goto out;
	}
	
	if (connect(fdr->fd, addr->ai_addr, addr->ai_addrlen) < 0) {
		fdr->errval = errno;
		fdr->strerror = strerror;
		close(fdr->fd);
		fdr->fd = -1;
	}
out:
	if (addr) freeaddrinfo(addr);
	return fdr;
}

