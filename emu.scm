(define-module emu
 ; (export dump-registers create-emu get-code8 get-sign-code32
;	  get-sign-code8 get-code32 register-name emu-add)
   (export-all)
  )
(select-module emu)

(use gauche.uvector)
(use gauche.collection)

(define-class <registers> ()
  ((eax :init-keyword :eax :init-value 0)
   (ecx :init-keyword :ecx :init-value 0)
   (edx :init-keyword :edx :init-value 0)
   (ebx :init-keyword :ebx :init-value 0)
   (esp :init-keyword :esp :init-value 0)
   (ebp :init-keyword :ebp :init-value 0)
   (esi :init-keyword :esi :init-value 0)
   (edi :init-keyword :edi :init-value 0))
  )
(define register-name '(eax ecx edx ebx esp ebp esi edi))

(define-constant ESP 4)
(define-constant EBP 5)

(define-constant CARRY-FLAG 1)
(define-constant ZERO-FLAG (ash 1 6))
(define-constant SIGN-FLAG (ash 1 7))
(define-constatn OVERFLOW-FLAG (ash 1 1))

(define (num8->2complement num)
  (+ (logxor num #b11111111) 1))

(define (num32->2complement num)
  (+ (logxor num (- (expt 2 32) 1) ) 1))

(define (get-register-number1 name n lis)
  (if (eq? name (car lis))
    n
    (get-register-number1 name (+ n 1) (cdr lis))))

(define (get-register-number name)
  (get-register-number1 name 0 register-name))

(define-class <emulator> ()
  ((memory :init-keyword :memory)
   (registers :init-keyword :registers)
   (eflags :init-keyword :eflags)
   (eip :init-keyword :eip))
  )

(define-method dump-registers ((reg <registers>))
	(for-each (lambda (s)
		    (format #t "~a = ~8,'0x\n" s (ref reg s))) register-name))
(define-method set-register32 ((reg <registers>) index value)
	       (set! (ref reg (list-ref register-name index)) value))

(define-method get-register32 ((reg <registers>) index)
	       (ref reg (list-ref register-name index)))


(define-method dump-registers ((emu <emulator>))
	       (dump-registers (ref emu 'registers))
	       (format #t "~a = ~8,'0x\n" 'eip (ref emu 'eip)))

(define-method set-register32 ((emu <emulator>) index value)
	       (set-register32 (ref emu 'registers) index value))

(define-method get-register32 ((emu <emulator>) index)
	       (get-register32 (ref emu 'registers) index))

(define-method eip-add ((emu <emulator>) n)
	       (set! (ref emu 'eip) (+ (ref emu 'eip) n)))

(define-method dump-memory ((emu <emulator>) min max)
       (let loop ([i min])
	 (when (< i max)
	   (format #t "~8,'0x : ~4,'0x\n"
		   i
		   (get-memory8 emu i))
	   (loop (+ i 1)))))

;(define-method 

(define (create-emu size eip esp)
  (make <emulator> :memory (make-u8vector size 0)
	           :registers (make <registers> :esp esp)
		   :eip eip))

(define (get-code8 emu index)
  (let ((memory (ref emu 'memory))
	(eip (ref emu 'eip)))
    (u8vector-ref memory (+ index eip))))


(define (get-sign-code8-1 num)
  (if (not (= (logand #b10000000 num) 0))
    (- (+ (logxor num #b11111111) 1))
    num))

(define (get-sign-code32-1 num)
  (if (not (= (logand (expt 2 31) num) 0))
    (- (+ (logxor num (- (expt 2 32) 1) ) 1))
    num))

(define (get-sign-code32 emu index)
  (get-sign-code32-1 (get-code32 emu index)))

(define (get-sign-code8 emu index)
  (get-sign-code8-1 (get-code8 emu index)))

(define (get-code32 emu index)
  (let loop ((i 0) (ret 0))
    (if (< i 4)
      (loop (+ i 1) (logior ret 
			   (ash (get-code8 emu (+ index i)) 
				(* i 8))))  
      ret)))
(define (set-memory8 emu address value)
  (let ([memory (ref emu 'memory)])
    (set! (ref memory address) (logand value #xff))))

(define (set-memory32 emu address value)
  (dotimes (i 4)
    (set-memory8 emu (+ address i) (ash value (* i -8)))))

(define (get-memory8 emu address)
  (ref (ref emu 'memory) address))

(define (get-memory32 emu address)
  (let loop ((i 0) (ret 0))
    (if (< i 4)
      (loop (+ i 1) 
	    (logior ret (ash (get-memory8 emu (+ address i)) (* i 8))))
      ret)))


(define (push32 emu value)
  (let* ([esp (get-register-number 'esp)]
	 [address (- (get-register32 emu esp) 4)])
    (set-register32 emu esp address)
    (set-memory32 emu address value)))

(define (pop32 emu)
  (let* ([esp (get-register-number 'esp)]
	 [address (get-register32 emu esp)]
	 [ret (get-memory32 emu address)])
    (set-register32 emu esp (+ address 4))
    ret))

(define (update-eflags-sub emu v1 v2 result)
  (let* ([sign1 (ash v1 -31)]
	 [sign2 (ash v2 -31)]
	 [signr (logand (ash result -31) 1)])
    (set-carry emu (not (zero? (ash result -32))))
    (set-zero emu (zero? result))
    (set-sign emu (not (zero? signr)))
    (set-overflow emu (and (not (= sign1 sign2)) (not (= sign1 signr))))))

(define (make-set-flag flag)
  (lambda (emu is)
    (if is
      [set! (ref emu 'eflags) (logior (ref emu 'eflags) flag)]
      [set! (ref emu 'eflags) (logand (ref emu 'eflags) (lognot flag))])))

(define set-carry (make-set-flag CARRY-FLAG))
(define set-zero (make-set-flag ZERO-FLAG))
(define set-sign (make-set-flag SIGN-FLAG))
(define set-overflow (make-set-flag OVERFLOW-FLAG))

(provide "emu")
