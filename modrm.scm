(define-module modrm 
  (export opecode reg-index init parse-modrm 
	  set-rm32 get-rm32 set-r32 get-r32
	  calc-memory-address
	  <modrm>
	  ))
(select-module modrm)
(use emu)

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

(define-method init ((m <modrm>))
  (let1 slot-names (map slot-definition-name (class-slots <modrm>))
    (for-each (lambda (s) (set! (ref m s) 0)) slot-names)))

(define (parse-modrm emu mr)
  (let ([code (get-code8 emu 0)])
    (init mr)
    (set! (ref mr 'mod) (ash (logand code #xC0) -6))
    (set! (ref mr opecode) (ash (logand code #x38) -3))
    (set! (ref mr 'rm) (logand code #x07))
    
    (eip-add emu 1)
    
    ;sib読み込み
    (if (and (not (= (ref mr 'mod) 3)) (= (ref mr 'rm) 4))
      (begin (set! (ref mr 'sib) (get-code8 emu 0))
	     (eip-add emu 1)))

    (cond [(or (and (= (ref mr 'mod) 0)
		    (= (ref mr 'rm) 5))
	       (= (ref mr 'mod) 2))
	   (set! (ref mr 'disp32) (get-sign-code32 emu 0))
	   (eip-add emu 4)]
	  [(= (ref mr 'mod) 1)
	   (set! (ref mr 'disp8) (get-sign-code8 emu 0))
	   (eip-add emu 1)]
	  )

    
    ))
(define (set-rm32 emu modrm value)
  (if (= (ref modrm 'mod) 3)
    (set-register32 emu (ref modrm 'rm) value)
    (let1 address (calc-memory-address emu modrm)
      (set-memory32 emu address value))))

(define (get-rm32 emu modrm)
  (if (= (ref modrm 'mod) 3)
    (get-register32 emu (ref modrm 'rm))
    (get-memory32 emu (calc-memory-address emu modrm))))
 
(define (calc-memory-address-mod-0 emu modrm)
  (cond [(= (ref modrm 'rm) 4)
	 (error "not implemented ModRM mod = 0, rm = 4\n")]
	[(= (ref modrm 'rm) 5)
	 (ref modrm 'disp32)]
	[else
	  (get-register32 emu (ref modrm 'rm))]))

(define (calc-memory-address-mod-1 emu modrm)
  (cond [(= (ref modrm 'rm) 4)
	 (error "not implemented ModRM mod = 1, rm = 4\n")]
	[else
	  (+ (get-register32 emu (ref modrm 'rm)) (ref modrm 'disp8))]))

(define (calc-memory-address-mod-2 emu modrm)
  (if (= (ref modrm 'rm) 4)
    (error "not implemented ModRM mod = 1, rm = 4\n")
    (+ (get-register32 emu (ref modrm 'rm)) (ref modrm 'disp32))))


(define (calc-memory-address emu modrm)
  (let1 mod (ref modrm 'mod)
    (cond [(= mod 0) 
	   (calc-memory-address-mod-0 emu modrm)]
	  [(= mod 1)
	   (calc-memory-address-mod-1 emu modrm)]
	  [(= mod 2)
	   (calc-memory-address-mod-2 emu modrm)]
	  [else
	    (error "not implemented ModRM mod = 3\n")])))

(define (set-r32 emu modrm value)
  (set-register32 emu (ref modrm reg-index) value))

(define (get-r32 emu modrm)
  (get-register32 emu (ref modrm reg-index)))

