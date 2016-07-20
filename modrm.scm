(define-module modrm) 
;  (export ))
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

(define (parse_modrm emu mr)
  (let ([code (get-code8 emu 0)])
    (init mr)
    (set! (ref mr 'mod) (ash (logand code #xC0) -6))
    (set! (ref mr opecode) (ash (logand code #x38) -3))
    (set! (ref mr 'rm) (logand #x07))
    
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

