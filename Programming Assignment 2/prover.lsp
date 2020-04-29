;---------------------------------------------------------------------------
;---------------------------------------------------------------------------
; Theorems
;---------------------------------------------------------------------------
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; EXAMPLE THEOREMS WITH ANSWER PREDICATE					
;---------------------------------------------------------------------------------
(defvar *howling*   '((1 ((howl z)) ((hound z)))
                    (2 nil ((have x y) (cat y) (have x z) (mouse z)))
                    (3 nil ((ls w) (have w v) (howl v)))
                    (4 ((have (john) (a))) nil)
                    (5 ((cat (a)) (hound (a))) nil)
                    (6 ((Answer r) (mouse (b))) nil)    ;; goal clause begins here
                    (7 ((Answer r) (ls (john))) nil)
                    (8 ((Answer r) (have (john) (b))) nil)))

(defvar *rr* 	'( (1   ( (rr (a)) )   	( (coyote y) )   )
		   (2	( (chase z (a)) ) ( (coyote z) )   )	
		   (3   ( (smart x) )   ( (rr x) (beep x) )   )
		   (4   nil ( (coyote w) (rr u) (catch w u) (smart u) )    )
		   (5   ( (frustrated s) (catch s t) ) ( (coyote s) (rr t) 
				(chase s t) )    )
		   (6   ( (beep r) )  ( (rr r) )   )
		   (7   ( (Answer p) ( coyote (b))  ) nil   ) ;; goal clause begins here
		   (8   ((Answer p)) ( (frustrated (b)) )   )     ) )

(defvar *customs* '( (1 ( (v x) (s x (f x)) )
		      ( (e x) ) )
		   (2 ( (v y) (c (f y)) )
		      ( (e y) ) )
		   (3 ( (e (a)) ) nil )
		   (4 ( (d (a)) ) nil )
		   (5 ( (d z) )   ( (s (a) z) ) )
		   (6 ((Answer d))   ( (d w) (v w) ) )
		   (7 ((Answer d))  ( (d r) (c r) ) )   )    ) ;; goal clause begins here
		   		

(defvar *Harmoniaexp*  '((1 ((Grandparent x y)) ((Parent x z) (Parent z y)))
                    (2 ((Parent x y)) ((Mother x y)))
                    (3 ((Parent x y)) ((Father x y)))
                    (4 ((Father (Zeus) (Ares))) nil)
                    (5 ((Mother (Hera) (Ares))) nil)
					(6 ((Father (Ares) (Harmonia))) nil)
                    (7 ((Answer x)) ((Grandparent x (Harmonia))))))    ;; goal clause begins here				

(defvar *cae*   '((1 ((man (marcus))) nil  )
                    (2 ((roman (marcus))) nil  )
					(3 ((person x)) ((man x))  )
                    (4 ((ruler (caesar))) nil  )
                    (5 ((loyal x (caesar)) (hate x (caesar))) ((roman x)))
					(6 ((loyal x (f x))) nil )
					(7  nil ( (person x) (ruler y) (ta x y) (loyal x y) ) )
					(8  ( (ta (marcus) (caesar) ) ) nil )
                    (9  ((Answer x)) ((hate x (caesar)))))) 

(defvar *basic* '((1 ((hound y)) ((howl x)))
                (2 ((have z) (howl (a))) nil)
				(3 nil ((have w)))
				(4 ((howl (b))) ((hound k)))
				(5 ((Answer g)) ((hound p)))))				

;---------------------------------------------------------------------------------
; EXAMPLE THEOREMS WITHOUT ANSWER PREDICATE					
;---------------------------------------------------------------------------------

(defvar *howling1*   '((1 ((howl z)) ((hound z)))
                    (2 nil ((have x y) (cat y) (have x z) (mouse z)))
                    (3 nil ((ls w) (have w v) (howl v)))
                    (4 ((have (john) (a))) nil)
                    (5 ((cat (a)) (hound (a))) nil)
                    (6 ((mouse (b))) nil)    ;; goal clause begins here
                    (7 ((ls (john))) nil)
                    (8 ((have (john) (b))) nil)))		

(defvar *rr1* 	'( (1   ( (rr (a)) )   	( (coyote y) )   )
		   (2	( (chase z (a)) ) ( (coyote z) )   )	
		   (3   ( (smart x) )   ( (rr x) (beep x) )   )
		   (4   nil ( (coyote w) (rr u) (catch w u) (smart u) )    )
		   (5   ( (frustrated s) (catch s t) ) ( (coyote s) (rr t) 
				(chase s t) )    )
		   (6   ( (beep r) )  ( (rr r) )   )
		   (7   ( ( coyote (b)) ) nil   ) ;; goal clause begins here
		   (8   nil ( (frustrated (b)) )   )     ) )

(defvar *customs1* '( (1 ( (v x) (s x (f x)) )
		      ( (e x) ) )
		   (2 ( (v y) (c (f y)) )
		      ( (e y) ) )
		   (3 ( (e (a)) ) nil )
		   (4 ( (d (a)) ) nil )
		   (5 ( (d z) )   ( (s (a) z) ) )
		   (6 nil   ( (d w) (v w) ) )
		   (7 nil  ( (d r) (c r) ) )   )    ) ;; goal clause begins here
		
				
(defvar *Harmonia1*   '((1 ((Grandparent x y)) ((Parent x z) (Parent z y)))
                    (2 ((Parent x y)) ((Mother x y)))
                    (3 ((Parent x y)) ((Father x y)))
                    (4 ((Father (Zeus) (Ares))) nil)
                    (5 ((Mother (Hera) (Ares))) nil)
					(5 ((Father (Ares) (Harmonia))) nil)
                    (6 nil ((Grandparent x (Harmonia))))))    ;; goal clause begins here	
					
(defvar *cae1*   '((1 ((man (marcus))) nil  )
                    (2 ((roman (marcus))) nil  )
					(3 ((person x)) ((man x))  )
                    (4 ((ruler (caesar))) nil  )
                    (5 ((loyal x (caesar)) (hate x (caesar))) ((roman x)))
					(6 ((loyal x (f x))) nil )
					(7  nil ( (person x) (ruler y) (ta x y) (loyal x y) ) )
					(8  ( (ta (marcus) (caesar) ) ) nil )
                    (9  ((Answer x)) ((hate x (caesar)))))) 					

(defvar *basic1* '((1 ((hound y)) ((howl x)))
                (2 ((have z) (howl (a))) nil)
				(3 nil ((have w)))
				(4 ((howl (b))) ((hound k)))
				(5 nil ((hound p)))))

				

;----------------------------------------------------------------------------
; 
; MAIN PROVER FUNCTION ( TWO - POINTER THEOREM) 
;
;----------------------------------------------------------------------------
 (defun two-pointer (input negativeindex)
   (let ((outer-pointer (- negativeindex 1)))
   (let ((duplicate-input input))
   (loop   
	while (< outer-pointer (list-length duplicate-input)) ; will break out when outer-pointer will go out of the total listlength of clauses
	do 
   (progn
    (loop for inner-pointer from 0 to outer-pointer  ; inner-pointer loops from beginning of clause till before negative clause begins
	do
	(progn
    (if (not (eq nil (nth 1 (nth outer-pointer duplicate-input))))  ; Checking if positive literals present in the beginning of negated clause
	(let ((positive-literals (nth 1 (nth outer-pointer duplicate-input)) ))	; list of positive literals
	(let ((negative-literals (nth 2 (nth inner-pointer duplicate-input)) )); list of negative literals
	(dolist (indv-predicates positive-literals nil) ; going through all predicates in positve 
	  (dolist ( n negative-literals nil) ; iterating through negative 
		(if (eq (nth 0 n) (nth 0 indv-predicates) ) ; found resolvable complimentary predicates
		(progn	  
		(let ((unified-clause '()))
        (let ((unified-clause-inner (nth inner-pointer duplicate-input)))
	    (let ((unified-clause-outer (nth outer-pointer duplicate-input)))  				  
	    (setq unified (unify n indv-predicates)) ; ((Z B) (y a) )     ; unify predicates to find the unifier
			  
		(progn
		(dolist (v unified nil)   ; iterate through the unifier
			(progn
		    (setq unified-clause-inner (sublis (list v) unified-clause-inner))
		    (setq unified-clause-outer (sublis (list v) unified-clause-outer))
			(setq n (sublis (list v) n))
			)
		)
					 
		(let ((first-index (+ (list-length duplicate-input) 1) ))    ; define positon number of newly generated clause
		(let ((resolve-clause (remove n (nth 1 unified-clause-outer):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals (union (nth 1 unified-clause-inner) resolve-clause :test #'equal) )) ; Appends both clauses post resolving the complimentary literal
        (let ((second-index (remove-duplicates mergeliterals :test #'equal)))	; forms postive literals of resolved 
		(let ((resolve-clause1 (remove n (nth 2 unified-clause-inner):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals1 (union resolve-clause1 (nth 2 unified-clause-outer):test #'equal) )) ; Appends both clauses post resolving the complimentary literal
		(let ((third-index (remove-duplicates mergeliterals1 :test #'equal)))
					   
		(setq new-clause (list first-index second-index third-index)) ; Generates newly resolved clause
		
		(if (and (eq nil (nth 1 new-clause) ) (eq nil (nth 2 new-clause) ))  ; when solution resolves to false (theorem proved)
		   (progn
		   (format t "~% RESOLVING CLAUSE 1  : ~s" (+ outer-pointer 1))
		   (format t "~% RESOLVING CLAUSE 2 : ~s" (+ inner-pointer 1))
		   (format t "~% LAST CLAUSE : ~s" new-clause)
		   (return-from two-pointer ( format t "~% THEOREM IS RESOLVED TO - : ~s" (nth 1 new-clause)))
			)
		)
			 
			 
		(if (and ( eq (nth 0 (nth 0 (nth 1 (nth 6 *Howling*)))) (nth 0 (nth 0 (nth 1 new-clause)))) (eq nil (nth 2 new-clause) ))  ; when solution resolves to false (theorem proved)
		(if (eq 1 (list-length (nth 1 new-clause)))
		   (progn
		   (format t "~% RESOLVING CLAUSE 1  : ~s" (+ outer-pointer 1))
		   (format t "~% RESOLVING CLAUSE 2 : ~s" (+ inner-pointer 1))
		   (format t "~% LAST CLAUSE : ~s" new-clause)
		   (return-from two-pointer ( format t "~% ANSWER TO QUERY IS : ~s" (nth 1(nth 0 (nth 1 new-clause)))))
		   
			))
		)
		; Check duplicates
		(let ((cnt 0))
		(progn
		(dolist (d duplicate-input nil)
        (if (dupe d new-clause)  ; if generated clause already present
        (setq cnt (+ 1 cnt))))   ; increment counter
        (if (eq cnt 0)       ; Append to clause list only if clause generated is unique
            (progn 				  
		    (setq duplicate-input (append duplicate-input (list new-clause)))
			(format t "~% RESOLVING CLAUSE 1 : ~s" (+ 1 inner-pointer))
		    (format t "~% RESOLVING CLAUSE 2 : ~s" (+ 1 outer-pointer))
		    (format t "~% NEW RESOLVED CLAUSE  : ~s" new-clause)
		     )
		)
		(setq cnt 0))))) ))))))))))))))))
				  
				  
		(if (not (eq nil (nth 2 (nth outer-pointer duplicate-input))))  ; Checking if negative literals present in the beginning of negated clause
		(let ((positive-literals (nth 1 (nth inner-pointer duplicate-input)) ))	; list of positive literals
	    (let ((negative-literals (nth 2 (nth outer-pointer duplicate-input)) )); list of negative literals	 
		(dolist (indv-predicates negative-literals nil) ; going through all predicates in negative 
		  (dolist ( p positive-literals nil) ; iterating through positive 
			(if (eq (nth 0 p) (nth 0 indv-predicates) ) ; found the complimentary
			(progn
            (let ((unified-clause '()))
		    (let ((unified-clause-inner (nth inner-pointer duplicate-input)))
		    (let ((unified-clause-outer (nth outer-pointer duplicate-input)))
		    (setq unified (unify p indv-predicates)) ; ((Z B) (y a) )
			
		    (progn
			(dolist (v unified nil)  ; iterate through the unifier
				(progn
			    (setq unified-clause-inner (sublis (list v) unified-clause-inner))
			    (setq unified-clause-outer (sublis (list v) unified-clause-outer))
				(setq p (sublis (list v) p))
				)
			)
					 
		(let ((first-index (+ 1 (list-length duplicate-input)) ))    ; define position number of newly generated clause
		(let ((resolve-clause (remove p (nth 1 unified-clause-inner):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals (union resolve-clause (nth 1 unified-clause-outer):test #'equal) )) ; Appends both clauses post resolving the complimentary literal
        (let ((second-index (remove-duplicates mergeliterals :test #'equal)))	; forms postive literals of resolved 
		(let ((resolve-clause1 (remove p (nth 2 unified-clause-outer):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals1 (union (nth 2 unified-clause-inner) resolve-clause1 :test #'equal) )) ; Appends both clauses post resolving the complimentary literal
		(let ((third-index (remove-duplicates mergeliterals1 :test #'equal)))		
		
	    (setq new-clause (list  first-index second-index third-index )) ; Generates newly resolved clause
		
		(if (and (eq nil (nth 1 new-clause) ) (eq nil (nth 2 new-clause) ))  ; when solution resolves to false (theorem proved)
		   (progn
		   (format t "~% RESOLVING CLAUSE 1  : ~s" (+ outer-pointer 1))
		   (format t "~% RESOLVING CLAUSE 2 : ~s" (+ inner-pointer 1))
		   (format t "~% LAST CLAUSE : ~s" new-clause)
		   (return-from two-pointer ( format t "~% THEOREM IS RESOLVED TO - : ~s" (nth 1 new-clause)))
			)
		)
		(if (and (eq (nth 0 (nth 0 (nth 1 (nth 6 *howling*)))) (nth 0 (nth 0 (nth 1 new-clause)))) (eq nil (nth 2 new-clause))) 
		(if (eq 1 (list-length (nth 1 new-clause)))
		   (progn
		   (format t "~% RESOLVING CLAUSE 1  : ~s" (+ outer-pointer 1))
		   (format t "~% RESOLVING CLAUSE 2 : ~s" (+ inner-pointer 1))
		   (format t "~% LAST CLAUSE : ~s" new-clause)
		   (return-from two-pointer ( format t "~% ANSWER TO QUERY IS : ~s" (nth 1(nth 0 (nth 1 new-clause)))))
			))
		)
		; Check duplicates
		(let ((cnt 0))
		(progn
		(dolist (d duplicate-input nil)
        (if (dupe d new-clause)  ; if generated clause already present
        (setq cnt (+ 1 cnt))))   ; increment counter
        (if (eq cnt 0)       ; Append to clause list only if clause generated is unique
            (progn 				  
		    (setq duplicate-input (append duplicate-input (list new-clause)))
			(format t "~% RESOLVING CLAUSE 1 : ~s" (+ 1 inner-pointer))
		    (format t "~% RESOLVING CLAUSE 2 : ~s" (+ 1 outer-pointer))
		    (format t "~% NEW RESOLVED CLAUSE  : ~s" new-clause)
		     )
		)
		(setq cnt 0)))
		
		)) ))))))))))))))))
		))
	  (setq outer-pointer (+ outer-pointer 1)))))))
				  
				  
;----------------------------------------------------------------------------
;---------------------------------------------------------------------------
; UNIT PREFERENCE STRATEGY
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------				  

(defun unit-preference (input)

   (let ((p1 0))  ; counter to increment p1 loop (only for unit clauses)
   (let (temp)
   (let ((duplicate-input (sorted-clause input))) ; Has original set of clauses sorted according to list length in ascending order
   (let ((resolved-new '())) ; Newly resolved clauses added in this list
   
   (loop 
     while(= 1 1)  ; True while statement
     do
      (progn
	  (setq temp 0)
      (dolist	(ap (nth 1 (nth p1 duplicate-input)))(if (eq 'Answer (car ap))(setq temp (+ temp 1))))
      (dolist	(an (nth 2 (nth p1 duplicate-input)))(if (eq 'Answer (car an))(setq temp (+ 1 temp))))	
      (loop   	   
		while (< (- (+ (list-length (nth 1 (nth p1 duplicate-input))) (list-length (nth 2 (nth p1 duplicate-input)))) temp) 2) ;outer loop		;loop p1 pointer for all unit literal clauses
		do
		(progn
		(loop for p2 from 1 to (- (list-length duplicate-input) 1)  ; p2 pointer loops from 2nd clause in list to end of the sorted list
		do                                                                                
	    (progn

        (if (not (eq nil (nth 1 (nth p1 duplicate-input))))   ;  ; Checking if positive literals present 
		(let ((positive-literals (nth 1 (nth p1 duplicate-input)) ))	; list of positive literals
		(let ((negative-literals (nth 2 (nth p2 duplicate-input)) )); list of negative literals
	    (dolist (indv-predicates positive-literals nil) ; going through all predicates in positve 
		 (dolist ( n negative-literals nil) ; iterating through negative 
			(if (eq (car n) (car indv-predicates) ) ;  found resolvable complimentary predicates
			(progn
            (let ((unified-clause-inner (nth p2 duplicate-input)))
		    (let ((unified-clause-outer (nth p1 duplicate-input)))
			(let ((unip n))
            (progn 				  
		    (setq unified (unify n indv-predicates)) ; ((Z B) (y a) )
		    (dolist (v unified nil)
				(progn
				(setq unip (sublis (list v) unip)) 
				(setq unified-clause-inner (sublis (list v) unified-clause-inner))
			    (setq unified-clause-outer (sublis (list v) unified-clause-outer))
			    
				)
			)

        (let ((first-index (+ 1 (+ (list-length duplicate-input) (list-length resolved-new))) ))    ; define position number of newly generated clause
		(let ((resolve-clause (remove unip (nth 1 unified-clause-outer):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals (union (nth 1 unified-clause-inner) resolve-clause :test #'equal) )) ; Appends both clauses post resolving the complimentary literal
        (let ((second-index (remove-duplicates mergeliterals :test #'equal)))	; forms postive literals of resolved 
		(let ((resolve-clause1 (remove unip (nth 2 unified-clause-inner):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals1 (union resolve-clause1 (nth 2 unified-clause-outer):test #'equal) )) ; Appends both clauses post resolving the complimentary literal
		(let ((third-index (remove-duplicates mergeliterals1 :test #'equal)))		
		
	    (setq new-clause (list  first-index second-index third-index )) ; Generates newly resolved clause

       (if (and (eq nil (nth 1 new-clause) ) (eq nil (nth 2 new-clause)))
		    (progn
			(format t "~% RESOLVING CLAUSE 1  : ~s" (car (nth p1 duplicate-input)))
	    	(format t "~% RESOLVING CLAUSE 2 : ~s" (car (nth p2 duplicate-input)))
			(format t "~% LAST CLAUSE : ~s" new-clause)
		   (return-from unit-preference ( format t "~% THEOREM IS RESOLVED TO - : ~s" (nth 1 new-clause)))
			
			)
		)
		(if (and ( eq (nth 0 (nth 0 (nth 1 (nth 6 *Howling*)))) (nth 0 (nth 0 (nth 1 new-clause)))) (eq nil (nth 2 new-clause) ))  ; when solution resolves to false (theorem proved)
	    (if (eq (+ (list-length (second new-clause)) (list-length (third new-clause))) 1)
		    (progn
			(format t "~% RESOLVING CLAUSE 1  : ~s" (car (nth p1 duplicate-input)))
	    	(format t "~% RESOLVING CLAUSE 2 : ~s" (car (nth p2 duplicate-input)))
			(format t "~% LAST CLAUSE : ~s" new-clause)
		    (return-from unit-preference ( format t "~% ANSWER TO QUERY IS : ~s"(nth 1(nth 0 (nth 1 new-clause)))))
			)))
			
			
		(let ((cnt 0))
		(progn
		(dolist (d duplicate-input nil)
	    (if (dupe d new-clause) ; if generated clause already present in orginal list
          (setq cnt (+ 1 cnt)))) ; increment counter
		  (dolist (e resolved-new nil) ; if generated clause already present in newly-generated  list
             (if (dupe e new-clause)
              (setq cnt (+ 1 cnt)))) ; increment counter
              (if (eq cnt 0)    ; Append to clause list only if clause generated is unique
              (progn 				  
		      (setq resolved-new (append resolved-new (list new-clause)))
			  (format t "~% RESOLVING CLAUSE A  : ~s" (car (nth p1 duplicate-input)))
			  (format t "~% RESOLVING CLAUSE B  : ~s" (car (nth p2 duplicate-input)))
			  (format t "~% NEW RESOLVED CLAUSE : ~s" new-clause)
		    )
		)
		(setq cnt 0)))
					 )
					 )))))				  
				  ))))))))))))

		(if (not (eq nil (nth 2 (nth p1 duplicate-input))))    ;  ; Checking if negative literals present 
		(let ((positive-literals (nth 1 (nth p2 duplicate-input)) ))	; list of positive literals
		(let ((negative-literals (nth 2 (nth p1 duplicate-input)) )); list of negative literals
		(dolist (indv-predicates negative-literals nil) ; going through all predicates in negative 
		(dolist ( p positive-literals nil) ; iterating through positive literals
		   (if (eq (car indv-predicates) (car p) ) ; found the complimenatery
			(progn
			(let ((unip p))
		    (let ((unified-clause-inner (nth p2 duplicate-input)))
			(let ((unified-clause-outer (nth p1 duplicate-input)))
		    (progn 				  
		    (setq unified (unify p indv-predicates)) ; ((Z B) (y a) )
		    (dolist (v unified)
				(progn
				(setq unip (sublis (list v) unip))
				(setq unified-clause-inner (sublis (list v) unified-clause-inner))
			    (setq unified-clause-outer (sublis (list v) unified-clause-outer))
		   ))
	    (let ((first-index (+ 1 (+ (list-length duplicate-input) (list-length resolved-new))) ))    ; define position number of newly generated clause
		(let ((resolve-clause (remove unip (nth 1 unified-clause-inner):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals (union (nth 1 unified-clause-outer) resolve-clause :test #'equal) )) ; Appends both clauses post resolving the complimentary literal
        (let ((second-index (remove-duplicates mergeliterals :test #'equal)))	; forms postive literals of resolved 
		(let ((resolve-clause1 (remove unip (nth 2 unified-clause-outer):test #'equal) )) ; resolves the clause by eleminating the complimentary literal
		(let ((mergeliterals1 (union resolve-clause1 (nth 2 unified-clause-inner):test #'equal) )) ; Appends both clauses post resolving the complimentary literal
		(let ((third-index (remove-duplicates mergeliterals1 :test #'equal)))		
		
	    (setq new-clause (list  first-index second-index third-index )) ; Generates newly resolved clause

		(if (and (eq nil (nth 1 new-clause) ) (eq nil (nth 2 new-clause)))
		    (progn
			(format t "~% RESOLVING CLAUSE 1  : ~s" (car (nth p1 duplicate-input)))
	    	(format t "~% RESOLVING CLAUSE 2 : ~s" (car (nth p2 duplicate-input)))
			(format t "~% LAST CLAUSE : ~s" new-clause)
		    (return-from unit-preference ( format t "~% THEOREM IS RESOLVED TO - : ~s" (nth 1 new-clause)))
			)
		)
		(if (and ( eq (nth 0 (nth 0 (nth 1 (nth 6 *Howling*)))) (nth 0 (nth 0 (nth 1 new-clause)))) (eq nil (nth 2 new-clause) ))  ; when solution resolves to false (theorem proved)
	    (if (eq (+ (list-length (second new-clause)) (list-length (third new-clause))) 1)
		    (progn
			(format t "~% RESOLVING CLAUSE 1  : ~s" (car (nth p1 duplicate-input)))
	    	(format t "~% RESOLVING CLAUSE 2 : ~s" (car (nth p2 duplicate-input)))
			(format t "~% LAST CLAUSE : ~s" new-clause)
		    (return-from unit-preference ( format t "~% ANSWER TO QUERY IS : ~s"(nth 1(nth 0 (nth 1 new-clause)))))
			
			)))
		(let ((cnt 0))
		(progn
		(dolist (d duplicate-input nil)
	    (if (dupe d new-clause) ; if generated clause already present in orginal list
          (setq cnt (+ 1 cnt)))) ; increment counter
		  (dolist (e resolved-new nil) ; if generated clause already present in newly-generated  list
             (if (dupe e new-clause)
              (setq cnt (+ 1 cnt)))) ; increment counter
              (if (eq cnt 0)    ; Append to clause list only if clause generated is unique
              (progn 				  
		      (setq resolved-new (append resolved-new (list new-clause)))
			  (format t "~% RESOLVING CLAUSE C  : ~s" (car (nth p1 duplicate-input)))
			  (format t "~% RESOLVING CLAUSE D  : ~s" (car (nth p2 duplicate-input)))
			  (format t "~% NEW RESOLVED CLAUSE : ~s" new-clause)
		    )
		)
		(setq cnt 0)))
		
					  )
					 ))))				  
				    )))
					))))))))))
					
					))
					
		(setq p1 (+ p1 1)) ; increment p1 pointer to move to next unit literal
		(setq temp 0)
        (dolist	(ap (nth 1 (nth p1 duplicate-input)))
	        (if (eq 'Answer (car ap))(setq temp (+ 1 temp))))
        (dolist	(ans-n (nth 2 (nth p1 duplicate-input)))
		    (if (eq 'Answer (car ans-n))(setq temp (+ temp 1))))
		))
		
		(setq duplicate-input (append duplicate-input resolved-new))
		(setq duplicate-input (sorted-clause duplicate-input))     ; RE-SORT the list with newly generated clauses
				   
		(if (eq 0 (list-length resolved-new))   
		(return-from unit-preference "THEOREM CANNOT BE RESOLVED"))
		(setq p1 0)
		(setq resolved-new '())  ; Reset the resolved clause to null list
		 
		)	 ))))))

;---------------------------------------------------------------------------
;
; sorting clauses based on number of literals
;
;---------------------------------------------------------------------------

(defun sorted-clause (clause)
(let ((sorted-length '()))
   (progn
   (dolist (c clause nil)
     (let ((temp 0))
   (dolist	(ap (second c))
  (if (eq 'Answer (car ap))(setq temp (+ temp 1))))
  (dolist	(an (nth 2 c))
  (if (eq 'Answer (car an))(setq temp (+ temp 1))))
      (setq lengcl (- (+ (list-length (nth 1 c)) (list-length (nth 2 c))) temp)))
      (setf sorted-length (append sorted-length (list (list (car c) lengcl)))))    
	  (setf sorted-length  (sort sorted-length (lambda (x y) (< (second x) (second y)))))
	  (let ((sorted-clause '()))
	  (setq clause (sort clause (lambda (x y) (< (first x) (first y)))))
	  (dolist (leng sorted-length)
	  (setf sorted-clause (append sorted-clause (list (nth (- (car leng) 1) clause)))) 
	  )
	  (return-from sorted-clause sorted-clause)
	  )
	  ))	)		

(defun varsin (exp) (varsinb exp nil))

(defun varsinb (exp vars)
   (if (consp exp)
       (varsinc (cdr exp) vars)
       (if (member exp vars) vars (cons exp vars))))

(defun varsinc (args vars)
   (if args
       (varsinc (cdr args) (varsinb (car args) vars))
       vars) )

; Unify literals u and v, assumed to have no variables in common.
; Result is a substitution alist or ((T . T)) , suitable for sublis .
; Note that (X . (A)) will print as (X A) .
(defun unify (u v)
  (let ((*u* (copy-tree u))
        (*v* (copy-tree v)) *subs*)
      (declare (special *u* *v* *subs*))
    (if (unifyb *u* *v*) (or *subs* (list (cons t t)))) ))

(defun unifyb (u v)
  (cond ((eq u v))
        ((symbolp u) (varunify v u))
        ((symbolp v) (varunify u v))
        ((and (consp u) (consp v)
              (eq (car u) (car v))
	      (eql (length (cdr u))
		   (length (cdr v))))
          (every #'unifyb (cdr u) (cdr v)) )) )

(defun varunify (term var)
    (declare (special *u* *v* *subs*))
  (unless (occurs var term)
    (dolist (pair *subs*)
      (setf (cdr pair)
	    (subst term var (cdr pair))))
    (nsubst term var *u*)
    (nsubst term var *v*)
    (push (cons var term) *subs*)))

(defun occurs (var form)
  (if (consp form)
      (or (occurs var (car form))
	  (occurs var (cdr form)))
      (eql var form)))


(defun dupe (u v)
   (and (identical (second u) (second v))
        (identical (third u) (third v))))

;----------------------------------------------------------------------------
; 
; identical : check if two set of predicates are literally the same
;
;----------------------------------------------------------------------------
(defun identical (u v) 
   (if (not (eq (length u) (length v)))
      nil
      (dolist (pred u T)
         (if (not (member pred v :test 'equal)) 
            (return-from identical nil)
	  )
      )
   )
)	  