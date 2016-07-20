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
    (eip-add 1)
    (parse_modrm emu mr)
    )
  )
(define (get-instructions code)
  (cond [(and (<= #xb8 code) (<= code (+ #xb8 7)))
	 mov-r32-imm32]
	[(= code #xeb) short-jump]
	[(= code #xe9) near-jump]
	[else '()]))

(provide "instruction")
