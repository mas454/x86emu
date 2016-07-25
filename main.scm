(add-load-path "." :relative)
(use emu)
(use instruction)
(use gauche.uvector)

(define memory-size (* 1024 1024))

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

(define (test-main)
  (set-memory32 test-emu 0 (expt 2 31))
  (print (get-memory32 test-emu 0)))

(define (main args)
  ;(emu-run (cadr args))
  (test-main) 
  0)

