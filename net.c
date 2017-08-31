
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include <net.h>

#define AF(name, fam) int af_##name = AF_##fam
#define SOCK(name, type) int sock_##name = SOCK_##type
#define IP(name, proto) uint32_t ip_##name = IPPROTO_##proto
#define IN(name, addr) uint32_t inaddr_##name = INADDR_##addr

AF(unix, UNIX);
AF(inet, INET);
AF(inet6, INET6);
AF(ipx, IPX);
AF(netlink, NETLINK);
AF(x25, X25);
AF(ax25, AX25);
AF(atmpvc, ATMPVC);
AF(packet, PACKET);
AF(alg, ALG);

SOCK(stream, STREAM);
SOCK(dgram, DGRAM);
SOCK(seqpacket, SEQPACKET);
SOCK(raw, RAW);
SOCK(rdm, RDM);

IP(ip, IP);
IP(icmp, ICMP);
IP(igmp, IGMP);
IP(ipip, IPIP);
IP(tcp, TCP);
IP(egp, EGP);
IP(pup, PUP);
IP(udp, UDP);
IP(idp, IDP);
IP(tp, TP);
IP(dccp, DCCP);
IP(ipv6, IPV6);
IP(rsvp, RSVP);
IP(gre, GRE);
IP(ah, AH);
IP(mtp, MTP);
IP(beetph, BEETPH);
IP(encap, ENCAP);
IP(pim, PIM);
IP(comp, COMP);
IP(sctp, SCTP);
IP(udplite, UDPLITE);
IP(mpls, MPLS);
IP(raw, RAW);

IN(any, ANY);
IN(broadcast, BROADCAST);
IN(none, NONE);
IN(loopback, LOOPBACK);
IN(unspec_group, UNSPEC_GROUP);
IN(allhosts_group, ALLHOSTS_GROUP);
IN(allrtrs_group, ALLRTRS_GROUP);
IN(max_local_group, MAX_LOCAL_GROUP);

ptr receive_offset(int sockfd, char *buf, int offset, size_t len, int flags) {
	int count = recv(sockfd, buf + offset, len, flags);
	int err = errno;
	if (count < 0) fputs(strerror(err), stderr);
	return Scons(Sinteger(count), Sinteger(err));
}

ptr send_offset(int sockfd, char *buf, int offset, size_t len, int flags) {
	int count = send(sockfd, buf + offset, len, flags | MSG_DONTWAIT);
	int err = errno;
	if (count < 0) fputs(strerror(err), stderr);
	return Scons(Sinteger(count), Sinteger(err));
}

ptr c_dial(int proto, const char *host, const char *serv) {
	struct addrinfo hints = {
		.ai_family = AF_UNSPEC,
		.ai_socktype = SOCK_STREAM,
		.ai_protocol = IPPROTO_TCP
	};
	struct addrinfo *res = 0;
	int err;
	int sock;

	err = getaddrinfo(host, serv, 0, &res);
	if (err) fputs(gai_strerror(err), stderr);

	sock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	err = errno;
	if (sock < 0) fputs(strerror(err), stderr);
	err = connect(sock, res->ai_addr, res->ai_addrlen);
	if (err) fputs(strerror(err), stderr);

	if (res) freeaddrinfo(res);
	return Scons(Sinteger(sock), Sinteger(err));
}

ptr socket_errno(int family, int type, int protocol) {
	return int_errno(socket(family, type, protocol));
}
