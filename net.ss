
(library (net)
  (export dial serve)
  (import (chezscheme))

  (define __ (load-shared-object "libnet.so"))

  (define ip:tcp (foreign-ref 'int (foreign-entry "ipproto_tcp") 0))
  (define ip:udp (foreign-ref 'int (foreign-entry "ipproto_udp") 0))

  (define c:strerror
    (foreign-procedure "strerror"
      (int) string))

  (define c:dial
    (foreign-procedure "c_dial" (int string string) ptr))

  (define (proto->number proto)
    (case proto
      [tcp ip:tcp]
      [udp ip:udp]
      [else (error 'proto->number "protocol not recognized" proto)]))

  (define dial
    (case-lambda
      [(proto host port) (dial proto host port #f)]
      [(proto host port transcoder)
       (let* ([sockfd (c:dial (proto->number proto) host port)])
         (open-fd-input/output-port (car sockfd) 'none transcoder))]))

  (define (serve host port fn)
    (error 'serve "not implemented"))
)
