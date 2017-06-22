
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
    c:socket c:getservbyname c:servent c:strerror
    c:receive-offset c:send-offset
    c:close c:shutdown)
  (import (chezscheme))

  (define lso
    (begin
      (for-each load-shared-object
        '("libc.so.6" "net.so"))))

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

  (define-ftype c:sockaddr
    (struct))

  (define-ftype c:addrinfo
    (struct
      [flags int]
      [family int]
      [type int]
      [proto int]
      [len size_t]
      [addr void*]
      [name (* char)]
      [next (* c:addrinfo)]))

  (define-ftype c:servent
    (struct
      [name (* char)]
      [aliases (* (* char))]
      [port (endian big unsigned-16)]
      [proto (* char)]))

  (define c:socket
    (foreign-procedure "socket_errno"
      (int int int) ptr))

  (define c:connect
    (foreign-procedure "connect"
      (int (* c:sockaddr) size_t) int))

  (define c:getservbyname
    (foreign-procedure "getservbyname"
      (string string) (* c:servent)))

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
)

(library (net)
  (export socket make-socket socket->port socket-connect)
  (import (chezscheme)
    (net c))

  (define-record-type socket (fields fd))

  (define unwrap-errno
    (case-lambda
      [(resp &cond) (unwrap-errno resp (lambda (x) (> x 0)) &cond)]
      [(resp test? &cond)
       (unless (test? (car resp))
         (raise (condition &cond (make-message-condition (c:strerror (cdr resp))))))
       (car resp)]))

  (define (socket-r! sock)
    (lambda (bv start n)
      (let* ([read+errno (c:receive-offset (socket-fd sock) bv start n)]
             [read (car read+errno)]
             [errno (cdr read+errno)])
        (if (zero? errno)
            read
            (error 'socket-r! (c:strerror errno))))))

  (define (socket-w! sock)
    (lambda (bv start n)
      (c:send-offset (socket-fd sock) bv start n)))

  (define (socket->port sock)
    (make-custom-binary-input/output-port
      (string-append "socket" " " (number->string (socket-fd sock)))
      (socket-r! sock)
      (socket-w! sock)
      #f
      #f
      (lambda ()
        (c:close (socket-fd sock))
        (c:shutdown (socket-fd sock)))))

  (define socket-connect
    (case-lambda
      [(unix-domain) (socket-connect)]
      [(host port) (socket-connect)]))
)
