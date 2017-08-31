
(library (net c)
  (export
    addr:any addr:broadcast addr:none addr:loopback
    addr:unspec-group addr:allhosts-group
    addr:allrtrs-group addr:max-local-group
    af:unix af:inet af:inet6
    ip:ip ip:icmp ip:igmp ip:ipip ip:tcp ip:egp ip:pup
    ip:udp ip:idp ip:tp ip:dccp ip:ipv6 ip:rsvp ip:gre
    ip:ah ip:mtp ip:beetph ip:encap ip:pim ip:comp
    ip:sctp ip:udplite ip:mpls ip:raw
    sock:stream sock:dgram sock:seqpacket
    sock:raw sock:rdm
    c:strerror c:dial c:receive-offset c:send-offset
    c:close c:shutdown)
  (import (chezscheme))

  (define lso
    (begin
      (for-each load-shared-object
        '("libc.so.6" "libnet.so"))))

  (define-syntax define-foreign-value
    (syntax-rules ()
      [(_ type (id name) ...)
       (begin
         (define id
           (foreign-ref type (foreign-entry name) 0)) ...)]))

  (define-foreign-value 'unsigned-32
    (addr:any "inaddr_any")
    (addr:broadcast "inaddr_broadcast")
    (addr:none "inaddr_none")
    (addr:loopback "inaddr_loopback")
    (addr:unspec-group "inaddr_unspec_group")
    (addr:allhosts-group "inaddr_allhosts_group")
    (addr:allrtrs-group "inaddr_allrtrs_group")
    (addr:max-local-group "inaddr_max_local_group")
    (af:unix "af_unix")
    (af:inet "af_inet")
    (af:inet6 "af_inet6")
    (af:ipx "af_ipx")
    (af:netlink "af_netlink")
    (af:x25 "af_x25")
    (af:ax25 "af_ax25")
    (af:atmpvc "af_atmpvc")
    (af:packet "af_packet")
    (af:alg "af_alg")
    (ip:ip "ip_ip")
    (ip:icmp "ip_icmp")
    (ip:igmp "ip_igmp")
    (ip:ipip "ip_ipip")
    (ip:tcp "ip_tcp")
    (ip:egp "ip_egp")
    (ip:pup "ip_pup")
    (ip:udp "ip_udp")
    (ip:idp "ip_idp")
    (ip:tp "ip_tp")
    (ip:dccp "ip_dccp")
    (ip:ipv6 "ip_ipv6")
    (ip:rsvp "ip_rsvp")
    (ip:gre "ip_gre")
    (ip:ah "ip_ah")
    (ip:mtp "ip_mtp")
    (ip:beetph "ip_beetph")
    (ip:encap "ip_encap")
    (ip:pim "ip_pim")
    (ip:comp "ip_comp")
    (ip:sctp "ip_sctp")
    (ip:udplite "ip_udplite")
    (ip:mpls "ip_mpls")
    (ip:raw "ip_raw")
    (sock:stream "sock_stream")
    (sock:dgram "sock_dgram")
    (sock:seqpacket "sock_seqpacket")
    (sock:raw "sock_raw")
    (sock:rdm "sock_rdm"))

  (define c:receive-offset
    (foreign-procedure "receive_offset"
      (int u8* int long int) ptr))

  (define c:send-offset
    (foreign-procedure "send_offset"
      (int u8* int long int) ptr))

  (define c:close
    (foreign-procedure "close"
      (int) int))

  (define c:shutdown
    (foreign-procedure "shutdown"
      (int int) int))

  (define c:strerror
    (foreign-procedure "strerror"
      (int) string))

  (define c:dial
    (foreign-procedure "c_dial" (int string string) ptr))
)

(library (net)
  (export socket make-socket socket->port dial serve)
  (import (chezscheme)
    (net c))

  (define-record-type socket (fields fd))

  (define unwrap-errno
    (case-lambda
      [(resp who) (unwrap-errno resp (lambda (x) (> x 0)) who)]
      [(resp test? who)
       (unless (test? (car resp))
         (raise (condition (make-who-condition who) (make-i/o-error) (make-message-condition (c:strerror (cdr resp))))))
       (car resp)]))

  (define (socket-r! sock)
    (lambda (bv start n)
      (unwrap-errno (c:receive-offset sock bv start n 0) 'socket-r!)))

  (define (socket-w! sock)
    (lambda (bv start n)
      (unwrap-errno (c:send-offset sock bv start n 0) 'socket-w!)))

  (define socket->port
    (case-lambda
      [(sock transcoder) (transcoded-port (socket->port sock) transcoder)]
      [(sock)
       (let ([fd (socket-fd sock)])
         (make-custom-binary-input/output-port
           (string-append "socket " (number->string fd))
           (socket-r! fd)
           (socket-w! fd)
           #f
           #f
           (lambda ()
             (c:close fd))))]))

  (define (dial host port)
    (let ([sockfd (unwrap-errno (c:dial 0 host port) 'dial)])
      (make-socket sockfd)))

  (define (serve host port fn)
    (error 'serve "not implemented"))
)
