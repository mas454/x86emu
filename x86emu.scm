(use gauche.uvector)
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
(define memory-size (* 1024 1024))

(define-class <emulator> ()
  ((memory :init-keyword :memory)
   (registers :init-keyword :registers)
   (eflags :init-keyword :eflags)
   (eip :init-keyword :eip))
  )

(define opecode 'op-reg)
(define reg-index 'op-reg)

(define-class <modrm> ()
  ((mod :init-value 0)
  ;(opecode)
  ;(reg-index)
   (op-reg :init-value 0)
   (rm :init-value 0)
   (sib :init-value 0)
   (disp8 :init-value 0)
   (disp32 :init-value 0)
   )
  )

;(define-method set-opecode ((mr <modrm>) val)
;	(set! (ref mr 'opecode) val)
;	(set! (ref mr 'reg-index) val))

;(define-method ref ((obj <modrm>) (slot <symbol>))
;  (if (or (eq? slot 'opecode) (eq? slot 'reg-index))
;    (next-method obj 'op-reg)
;    (next-method obj slot)))

;(define-method object-apply ((m <modrm>) (s <symbol>))
;  (ref m s))

;(define set-reg-index set-opecode)

(define mr (make <modrm>))
(define-method dump-registers ((reg <registers>))
	(for-each (lambda (s)
		    (format #t "~a = ~8,'0x\n" s (ref reg s))) register-name))

(define-method dump-registers ((emu <emulator>))
	       (dump-registers (ref emu 'registers))
	       (format #t "~a = ~8,'0x\n" 'eip (ref emu 'eip)))


(define (mov-r32-imm32 emu)
  (let1 reg (- (get-code8 emu 0) #xb8)
    (set! (ref (ref emu 'registers) (list-ref register-name reg)) (get-code32 emu 1))
    (set! (ref emu 'eip) (+ (ref emu 'eip) 5))))

(define (short-jump emu)
  (set! (ref emu 'eip) (+ (ref emu 'eip) (get-sign-code8 emu 1) 2)))

(define (near-jump emu)
  (set! (ref emu 'eip) (+ (ref emu 'eip) (get-sign-code32 emu 1) 5)))

(define (get-instructions code)
  (cond [(and (<= #xb8 code) (<= code (+ #xb8 7)))
	 mov-r32-imm32]
	[(= code #xeb) short-jump]
	[(= code #xe9) near-jump]
	[else '()]))

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

(define (run1 emu code)
  (let1 inst (get-instructions code)
	(format #t "eip = ~x, code = ~2,'0x\n" (ref emu 'eip) code)
	(cond [(null? inst) 
	       (format #t "\n\nNot Implemented: ~x\n" code)
	       '()]
	      [else (inst emu)
		    #t])))

(define (run-1 emu)
  (cond [(= (ref emu 'eip) #x00)
	 (display "\n\nend of program.\n\n")]  
        [(< (ref emu 'eip) memory-size)
	 (if (run1 emu (get-code8 emu 0))
	   (run-1 emu))]))

(define (run emu)
  (if (run1 emu (get-code8 emu 0))
    (run-1 emu)))

      

(define (memory-load emu p)
  (read-block! (ref emu 'memory) p #x7c00 (+ #x7c00 511)))

(define (emu-run file)
  (let [(emu (create-emu memory-size #x7c00 #x7c00))]
    (call-with-input-file file
      (lambda (p)
	(memory-load emu p)
	(run emu)
	(dump-registers emu)))))

(define test-emu (create-emu memory-size 0 0))

(define-method init ((m <modrm>))
  (let1 slot-names (map slot-definition-name (class-slots <modrm>))
    (for-each (lambda (s) (set! (ref m s) 0)) slot-names)))
