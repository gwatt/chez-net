
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
	return int_errno(recv(sockfd, buf + offset, len, flags));
}

ptr send_offset(int sockfd, char *buf, int offset, size_t len, int flags) {
	return int_errno(send(sockfd, buf + offset, len, flags));
}

ptr socket_errno(int family, int type, int protocol) {
	return int_errno(socket(family, type, protocol));
}
