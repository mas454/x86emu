(define-module instruction
  (export get-instructions))
(select-module instruction)
(use emu)
(use modrm)

(define (mov-r32-imm32 emu)
  (let1 reg (- (get-code8 emu 0) #xb8)
    (set! (ref (ref emu 'registers) (list-ref register-name reg)) (get-code32 emu 1))
    (set! (ref emu 'eip) (+ (ref emu 'eip) 5))))

(define (short-jump emu)
  (set! (ref emu 'eip) (+ (ref emu 'eip) (get-sign-code8 emu 1) 2)))

(define (near-jump emu)
  (set! (ref emu 'eip) (+ (ref emu 'eip) (get-sign-code32 emu 1) 5)))

(define (mov-rm32-imm32 emu)
  (let ([mr (make <modrm>)])
    (eip-add emu 1)
    (parse-modrm emu mr)
    (let1 value (get-code32 emu 0)
      (eip-add emu 4)
      (set-rm32 emu mr value)
    )
  ))

(define (mov-rm32-r32 emu)
  (eip-add emu 1)
  (let ([modrm (make <modrm>)])
    (parse-modrm emu modrm)
    (set-rm32 emu modrm (get-r32 emu modrm))))

(define (mov-r32-rm32 emu)
  (eip-add emu 1)
  (let ([modrm (make <modrm>)])
    (parse-modrm emu modrm)
    (set-r32 emu modrm (get-rm32 emu modrm))))

(define (add-rm32-r32 emu)
  (eip-add emu 1)
  (let ([modrm (make <modrm>)])
    (parse-modrm emu modrm)
    (let* ([r32 (get-r32 emu modrm)]
	   [rm32 (get-rm32 emu modrm)])
      (set-rm32 emu modrm (+ rm32 r32)))))

(define (sub-rm32-imm8 emu modrm)
  (let* ([rm32 (get-rm32 emu modrm)]
	 [imm8 (get-code8 emu 0)])
    ;(print rm32)
    (eip-add emu 1)
    (set-rm32 emu modrm (- rm32 imm8))))

(define (code-83 emu)
  (eip-add emu 1)
  (let ([modrm (make <modrm>)])
    (parse-modrm emu modrm)
    (cond [(= (ref modrm opecode) 5)
	   (sub-rm32-imm8 emu modrm)]
	  [else
	    (error (string-append "not implemented: 83 "
				  (number->string (ref modrm opecode))
				  "\n"))])))

(define (inc-rm32 emu modrm)
  (set-rm32 emu modrm (+ (get-rm32 emu modrm) 1)))



(define (code-ff emu)
  (eip-add emu 1)
  (let ([modrm (make <modrm>)])
    (parse-modrm emu modrm)
    (cond [(= (ref modrm opecode) 0)
	   (inc-rm32 emu modrm)]
	  [else 
	    (error (string-append "not implemented: FF "
				  (number->string (ref modrm opecode))
				  "\n"))]
	  )
    ))

(define (push-r32 emu)
  (let1 reg (- (get-code8 emu 0) #x50) 
    (push32 emu (get-register32 emu reg))
    (eip-add emu 1)))

(define (pop-r32 emu)
  (let1 reg (- (get-code8 emu 0) #x58)
    (set-register32 emu reg (pop32 emu))
    (eip-add emu 1)))

(define (get-instructions code)
  (cond 
    [(= code #x01) add-rm32-r32]
    [(= code #x83) code-83]
    [(= code #x89) mov-rm32-r32]
    [(= code #x8b) mov-r32-rm32]
    [(and (<= #xb8 code) (<= code (+ #xb8 7)))
	 mov-r32-imm32]
    [(= code #xc7) mov-rm32-imm32]
    [(= code #xeb) short-jump]
    [(= code #xe9) near-jump]
    [(= code #xff) code-ff]
    [else '()]))

(provide "instruction")
