#lang rosette

(require binaryio)
(require binaryio/reader)

(define-syntax-rule (while test body ...) ; while loop
    (let loop () (when test body ... (loop))))

(struct cpu
    (gprs) #:mutable #:transparent)

(struct machine
    (cpu ram) #:mutable #:transparent)

(define (gprs-get-x m idx)
    (vector-ref (cpu-gprs (machine-cpu m)) idx))

(println "start.")

; Get binary of program
(define program (open-input-file "sum.bin"))
(define b_program (make-binary-reader program))

; Read in all of memory from program
(while (not (b-at-limit/eof? b_program))
    (define next (b-read-le-int b_program 1))
    (fprintf (current-output-port) "prog: ~b~n" next))

; 32 gprs in the cpu and 100000000 ram
(define m (machine 
    (cpu (make-vector 32))
    (make-vector 100000000)))

(displayln (gprs-get-x m 8))

(define baseaddress 80000000)


(println "finished.")