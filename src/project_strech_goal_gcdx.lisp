;; NOTE: You do not need to understand the defpkg and
;; in-package commands.  They just let us reuse some symbols
;; that have been already taken by Common Lisp.

;; I don't import certain Common Lisp symbols, like PUSH and POP, because of their
;; definitions are incompatible with my intended definitions below.  Others of the
;; symbols I don't import are defined acceptably in the ACL2 package, e.g., NTH,
;; but I want to students to see their definitions as warm-up exercises.

(defpkg "M1"
  (set-difference-eq (union-eq *acl2-exports*
                               *common-lisp-symbols-from-main-lisp-package*)
                     '(push pop pc program step
                            nth update-nth nth-update-nth)))



(in-package "M1")

; Warm-up:  define the basic list processing functions needed to build the model.

(defun push (x y) (cons x y))
(defun top (stack) (car stack))
(defun pop (stack) (cdr stack))

(defun nth (n list)
  (if (zp n)
      (car list)
    (nth (- n 1) (cdr list))))

(defun update-nth (n v list)
  (if (zp n)
      (cons v (cdr list))
    (cons (car list)
          (update-nth (- n 1) v (cdr list)))))

(defun make-state (pc locals stack program)
     (cons pc
           (cons locals
                 (cons stack
                       (cons program
                             nil)))))

(defun pc (s) (nth 0 s))
(defun locals (s) (nth 1 s))
(defun stack (s) (nth 2 s))
(defun program (s) (nth 3 s))

(defun op-code (inst) (nth 0 inst))
(defun arg1 (inst) (nth 1 inst))
(defun arg2 (inst) (nth 2 inst))
(defun arg3 (inst) (nth 3 inst))

; The M1 Machine

(defun next-inst (s)
  (nth (pc s) (program s)))

; Now we define the semantics of each instruction.  These
; functions are called ``semantic functions.''

(defun execute-ILOAD (inst s)
  (make-state (+ 1 (pc s))
              (locals s)
              (push (nth (arg1 inst)
                         (locals s))
                    (stack s))
              (program s)))

(defun execute-ICONST (inst s)
  (make-state (+ 1 (pc s))
              (locals s)
              (push (arg1 inst) (stack s))
              (program s)))

(defun execute-IADD (inst s)
  (declare (ignore inst))
  (make-state (+ 1 (pc s))
              (locals s)
              (push (+ (top (pop (stack s)))
                       (top (stack s)))
                    (pop (pop (stack s))))
              (program s)))

(defun execute-ISUB (inst s)
  (declare (ignore inst))
  (make-state (+ 1 (pc s))
              (locals s)
              (push (- (top (pop (stack s)))
                       (top (stack s)))
                    (pop (pop (stack s))))
              (program s)))

(defun execute-IMUL (inst s)
  (declare (ignore inst))
  (make-state (+ 1 (pc s))
              (locals s)
              (push (* (top (pop (stack s)))
                       (top (stack s)))
                    (pop (pop (stack s))))
              (program s)))

(defun execute-ISTORE (inst s)
  (make-state (+ 1 (pc s))
              (update-nth (arg1 inst) (top (stack s)) (locals s))
              (pop (stack s))
              (program s)))

(defun execute-GOTO (inst s)
  (make-state (+ (arg1 inst) (pc s))
              (locals s)
              (stack s)
              (program s)))

(defun execute-IFEQ (inst s)
  (make-state (if (equal (top (stack s)) 0)
                  (+ (arg1 inst) (pc s))
                (+ 1 (pc s)))
              (locals s)
              (pop (stack s))
              (program s)))



 ;; Exercise 3:: define an operation IFLE that checks if (there
 ;; are at least two elements in the stack and) the first
 ;; element is less or equal to second, and if so, it goes
 ;; advances the PC by the amount provided in the argument
 ;; and pops those elements from the stack, else just
 ;; advances the PC to the next instruction. For example,
 ;; (IFLE -10) with the stack being [1 2 3 4] will result in
 ;; the PC being decremented by 10 and the stack being
 ;; updated to [3 4] (by popping 1 and 2) but the same
 ;; instruction with the stack [4 2 5] will advance the PC
 ;;  by 1 and no change in the stack.

(defun check-IFL-condition (stack)
( if (consp ( pop stack))
    ( if (< (top stack) (top (pop stack))) 
        t
        nil)
    nil )
)

;(defun execute-IFL (inst s)
;  (make-state (if ( check-IFL-condition (stack s))
;                  (+ (arg1 inst) (pc s))
;                (+ 1 (pc s)))
;              (locals s)
;              (if ( check-IFL-condition (stack s)) 
;                 (pop (pop (stack s)))
;                 (stack s)
;                 )
;              (program s)))
              
(defun execute-IFL (inst s)
  (make-state (if (check-IFL-condition (stack s))
                  (+ (arg1 inst) (pc s))
                (+ 1 (pc s)))
              (locals s)
              (pop (pop (stack s)))  ; Always pop two elements
              (program s)))

              
 ;; Exercise 4: Change the instruction do-inst appropriately, so that
 ;; m1-run understands the newly added instruction IFLE.

(defun do-inst (inst s)
  (if (equal (op-code inst) 'ILOAD)
      (execute-ILOAD  inst s)
      (if (equal (op-code inst) 'ICONST)
          (execute-ICONST  inst s)
          (if (equal (op-code inst) 'IADD)
              (execute-IADD   inst s)
              (if (equal (op-code inst) 'ISUB)
                  (execute-ISUB   inst s)
                  (if (equal (op-code inst) 'IMUL)
                      (execute-IMUL   inst s)
                      (if (equal (op-code inst) 'ISTORE)
                          (execute-ISTORE  inst s)
                          (if (equal (op-code inst) 'GOTO)
                              (execute-GOTO   inst s)
                              (if (equal (op-code inst) 'IFEQ)
                                  (execute-IFEQ   inst s)
                                  (if (equal (op-code inst) 'IFL)
                                      (execute-IFL   inst s)
                                      s))))))))))
                                  


(defun m1-step (s)
     (do-inst (next-inst s) s))

(defun haltedp (s)
  (equal (next-inst s) '(HALT)))

(defun m1-run (s n)
  (if (zp n)
      s
      (m1-run (m1-step s) (- n 1))))

;local variable 0 = n0 / divident
;local variable 1 = n1 /divisor



; local 0 = n0
; local 1 = n1.
; local 0 will contain the gcd

(defconst *gcd*
'(
  (ICONST 0)              ; 0 push 0 to stack
  (ISTORE 2)              ; 1 store 0 to var 2
      (ILOAD 1)           ; 2 push 1th local to stack
      (IFEQ 18)           ; 3 if n1 is 0 go to end
              (ILOAD 1)   ; 4 top of loop; push 1th local to stack
              (ILOAD 0)   ; 5 push 0th local to stack
              (IFL 6)     ; 6 if n0 < n1, goto 16 otherwise next instruction 
              (ILOAD 0)   ; 7, push n0(0th local) to stack
              (ILOAD 1)   ; 8, push n1(1th local) to stack
              (ISUB)      ; 9, (n0-n1)
              (ISTORE 0)  ; 10, n0 := n0-n1
              (GOTO -7)   ; 11
      (ILOAD 0)           ; 12 push n0 to stack
      (ISTORE 2)          ; 13 store it to var 2
      (ILOAD 1)           ; 14 push n1 to stack
      (ISTORE 0)          ; 15 store it to var 0
      (ILOAD 2)           ; 16 push var 2 to stack
      (ISTORE 1)          ; 17 store it var 1
      (ICONST 0)          ; 18 push 0 to stack
      (ISTORE 2)          ; 19 store 0 to var 2
      (GOTO -18)          ; 20
  (HALT )                 ; 21
))

(m1-run (make-state 0 '(20 2) nil *gcd*) 1)
 
      
(defun inner-lp-clk (n0 n1)
( if (and (natp n0)(natp n1)(not (zp n1)))
      (if (< n0 n1)
          3
          (+ 8 (inner-lp-clk (- n0 n1) n1)))    
      0 ))

      
(defun inner-lp-induction-plan (n0 n1 n2)
 ( if (and (natp n0)(natp n1)(not (zp n1)))    
    (if (< n0 n1)
      (list n0 n1 n2)
      (inner-lp-induction-plan (- n0 n1) n1 n2))
     nil ))

     
(defthm m1-run-split
( implies (and (natp p) (natp q))
          (equal (m1-run s (+ p q))  (m1-run (m1-run s p) q))))
       
(defthm m1-run-opener
(and (equal (m1-run s 0) s)
     (implies (natp i)
              (equal (m1-run s (+ 1 i))
                     (m1-run (m1-step s) i)))))

(defthm m1-step-opener
(and (implies (haltedp s) (equal (m1-step s) s))
     (implies (consp (next-inst s) )
              (equal (m1-step s) (do-inst (next-inst s) s)) )))

             
                
(include-book "arithmetic-5/top" :dir :system)

(defthm inner-loop-working
   (implies
    (and (natp n0) (natp n1) (natp n2) (not (zp n1)))
    (equal
     (m1-run (make-state 4 (list n0 n1 n2) nil *gcd*) (inner-lp-clk n0 n1))
     (make-state 12
                 (list (mod n0 n1) n1 n2)
                 nil
                 *gcd*)))
   :hints (("Goal"
            :induct (inner-lp-induction-plan n0 n1 n2)
            :do-not-induct t
            :do-not generalize)))
            


(defthm before-inner-loop-1
(implies (and (natp n0)(natp n1)(natp n2)(not (zp n1)))
         (equal (m1-run (make-state 2 (list n0 n1 n2 ) nil *gcd*) 2)
                ( make-state 4 (list n0 n1 n2) nil *gcd* ))))

(defthm before-inner-loop-2
(implies (and (natp n0)(natp n1)(zp n1)(natp n2))
         (equal (m1-run (make-state 2 (list n0 n1 n2 ) nil *gcd*) 2)
                ( make-state 21 (list n0 n1 n2) nil *gcd* ))))

(defthm after-inner-loop
(implies (and (natp n0)(natp n1)(natp n2))
         (equal (m1-run (make-state 12 (list n0 n1 n2 ) nil *gcd*) 9)
                ( make-state 2 (list n1 n0 0) nil *gcd* ))))
                      

(defun outer-lp-induction-plan (n0 n1 n2)
 ( if (and (natp n0)(natp n1)(natp n2))    
    (if (zp n1)
      (list n0 n1 0)
      (outer-lp-induction-plan n1 (mod n0 n1) 0))
     nil ))  
     

(defun clk-add (a b c d)
(+ a b c d))   

(defthm clk-add-run
( implies (and (natp a) (natp b) (natp c) (natp d) )
          (equal (m1-run s ( clk-add a b c d))  (m1-run (m1-run (m1-run (m1-run s a) b) c) d )))
          )

(defun outer-lp-clk (n0 n1)
( if (and (natp n0)(natp n1))
      (if (zp n1)
          2
          (clk-add 2 (inner-lp-clk n0 n1) 9 (outer-lp-clk n1 (mod n0 n1))  ))    
      0 ))
      
      
(defun my-gcd (a b)
  ( if ( and (natp a) (natp b))
  (if (zp b)
      a
      (my-gcd b (mod a b)))
     0 ))
      
      
      
(defthm outer-lp-clk-is-natp
  (implies (and (natp N0)        
                (natp N1) )       
               
           (natp (OUTER-LP-CLK N1 N0))))

(defthm inner-lp-clk-is-natp
  (implies (and (natp N0)        
                (natp N1) )       
               
           (natp (inner-lp-clk N1 N0))))
                 
 
 (defthm outer-loop-working-lemma
   (implies
    (and (natp n0) (natp n1) (natp n2) (not (zp n1) ))
    (equal
     (m1-run (make-state 2 (list n0 n1 n2) nil *gcd*) (outer-lp-clk n0 n1))
     (make-state 21
                 (list (my-gcd n0 n1) 0 0)
                 nil
                 *gcd*)))
   :hints (("Goal"
            :induct (outer-lp-induction-plan n0 n1 n2)
            :do-not-induct t
            :in-theory (disable clk-add  m1-run-opener m1-step-opener )
            )
            ("Subgoal *1/"
            :use ( (:instance inner-loop-working)
                   (:instance before-inner-loop-1)
                   (:instance before-inner-loop-2 (n0 n1) (n1 (mod n0 n1)) (n2 0) )
                   (:instance after-inner-loop (n0 (mod n0 n1)) (n1 n1) (n2 n2) )
                   ))))
                   
 (defthm before-outer-loop
 (implies (and (natp n0)(natp n1))
          (equal (m1-run (make-state 0 (list n0 n1) nil *gcd*) 2 )
                 (make-state 2 (list n0 n1 0) nil *gcd*))))                  

(defthm program-verified 
(implies (and (natp n0)(natp n1)(not (zp n1)))
          (equal (m1-run (make-state 0 (list n0 n1) nil *gcd*) (+ 2 (outer-lp-clk n0 n1) ))
                 (make-state 21
                 (list (my-gcd n0 n1) 0 0)
                 nil
                 *gcd*)))
                 :hints (("Goal"
                          :use ( (:instance before-outer-loop)
                                 (:instance outer-loop-working-lemma (n0 n0)(n1 n1)(n2 0)) ))))

