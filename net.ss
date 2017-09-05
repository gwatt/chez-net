
(library (net)
  (export dial serve)
  (import (chezscheme))

  (define __ (load-shared-object "libnet.so"))

  (define-ftype strerror-t (function (int) string))

  (define-ftype fd-result
    (struct
      [fd int]
      [errval int]
      [strerror (* strerror-t)]))

  (define ip:tcp (foreign-ref 'int (foreign-entry "ipproto_tcp") 0))
  (define ip:udp (foreign-ref 'int (foreign-entry "ipproto_udp") 0))

  (define c:dial
    (foreign-procedure "c_dial" (int string string (* fd-result)) (* fd-result)))

  (define (proto->number proto)
    (case proto
      [tcp ip:tcp]
      [udp ip:udp]
      [else (errorf 'proto->number "Unsupported protocol: ~s" proto)]))

  (define dial
    (case-lambda
      [(proto host port) (dial proto host port #f)]
      [(proto host port transcoder)
       (unless (string? host)
         (errorf 'dial "host must be a string ~s" host))
       (cond
         [(string? port)]
         [(and (integer? port) (exact? port) (< 0 port 65536))
          (set! port (number->string port))]
         [else (errorf 'dial "port must be a string or an exact integer between 0 and 65536: ~s" port)])
       (if transcoder
           (unless (transcoder? transcoder)
             (errorf 'dial "transcoder must be a valid transcoder or #f: ~s" transcoder)))

       (let* ([res (c:dial (proto->number proto) host port
                     (make-ftype-pointer fd-result (foreign-alloc (ftype-sizeof fd-result))))]
              [fd (ftype-ref fd-result (fd) res)]
              [errval (ftype-ref fd-result (errval) res)]
              [strerror (ftype-ref strerror-t () (ftype-ref fd-result (strerror) res))])
         (foreign-free (ftype-pointer-address res))
         (if (> 0 fd)
           (raise (condition (make-who-condition 'dial) (make-i/o-error) (make-message-condition (strerror errval)))))
         (open-fd-input/output-port fd 'none transcoder))]))

  (define (serve host port fn)
    (error 'serve "not implemented"))
)
