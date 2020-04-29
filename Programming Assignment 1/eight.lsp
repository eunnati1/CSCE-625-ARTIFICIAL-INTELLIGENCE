;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; 8-PUZZLE SEARCH USING BFS,DFS,IDS,GREEDY,A*,IDA*
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; global variable: goal state
;-------------------------------------------------------------------------------
(defvar *goal-state* '(1 2 3 8 0 4 7 6 5))
;-------------------------------------------------------------------------------
; global variables: number of tiles
;-------------------------------------------------------------------------------
(defvar *num-tile* 9)
(defvar *num-node* 0)
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; MAIN BFS FUNCTION IS CALLED WHICH INTERNALLY CALLS BFS-CORE FUNCTION TO PROCESS BFS SEARCH REQUEST
;-------------------------------------------------------------------------------
(defun bfs (state)
(let ((duplicate-check '()))
  (let ((node-count 1))
  (let ((node-list-length 0))
   (progn
     ;Returns the goal state and stores in bfs-result-node
	 (setq node-rep-lst (node-rep-list (node-rep state 0 0 nil)))
     (setq bfs-result-node (bfs-core node-rep-lst node-count duplicate-check node-list-length))
	 ;Prints path operation followed to reach the goal state
     (format t "~% BFS PATH IS - ~s" (reverse (fourth bfs-result-node)))
    )
   )
)))
;-------------------------------------------------------------------------------
; MAIN DFS FUNCTION IS CALLED WHICH INTERNALLY CALLS DFS-CORE FUNCTION TO PROCESS DFS SEARCH REQUEST
;-------------------------------------------------------------------------------

(defun dfs (state)
(let ((duplicate-check '()))
  (let ((node-count 1))
  (let ((node-list-length 0))
   (progn
     ;Returns the goal state and stores in dfs-result-node
	 (setq node-rep-lst (node-rep-list (node-rep state 0 0 nil)))
     (setq dfs-result-node (dfs-core node-rep-lst node-count duplicate-check node-list-length))
	 ;Prints path operation followed to reach the goal state
     (format t "~% DFS PATH IS - ~s" (reverse (fourth dfs-result-node)))
    )
   )
)))
;-------------------------------------------------------------------------------
; MAIN IDS FUNCTION IS CALLED WHICH INTERNALLY CALLS IDS-CORE FUNCTION TO PROCESS IDS SEARCH REQUEST
;-------------------------------------------------------------------------------

(defun ids (state)
(let ((duplicate-check '()))
  (let ((node-count 1))
  (let ((node-list-length 0))
  (let ((result-node '()))
  (setf depth-length 0)
  (progn
   (loop 
    (unless (not(equal (first result-node) *goal-state*)) (return-from ids (reverse (fourth result-node))))
    (setq node-rep-lst (node-rep-list (node-rep state 0 0 nil)))
    (setq result-node (ids-core node-rep-lst depth-length node-count duplicate-check node-list-length))
    (setq depth-length (+ 1 depth-length))
   )
  )
  ))
)))
;-------------------------------------------------------------------------------
; MAIN GREEDY FUNCTION IS CALLED WHICH INTERNALLY CALLS GREEDY-BEST-FIRST-CORE FUNCTION TO PROCESS BEST-FIRST SEARCH REQUEST
;-------------------------------------------------------------------------------
(defun greedy (state heuristic)
(let ((duplicate-check '()))
 (let ((node-count 1))
 (let ((node-list-length 0))
 (progn
   (setq node-rep-lst (node-rep-list (node-rep state 0 0 nil)))
   (setq result-node (greedy-core node-rep-lst heuristic node-count duplicate-check node-list-length))
   (format t "~% GREEDY-BEST-FIRST-SEARCH PATH IS - ~s" (reverse (fourth result-node)))
   )
))))
;-------------------------------------------------------------------------------
; MAIN A* FUNCTION IS CALLED WHICH INTERNALLY CALLS A*-CORE FUNCTION TO PROCESS A* SEARCH REQUEST
;-------------------------------------------------------------------------------
(defun Astar (state heuristic)
(let ((duplicate-check '()))
 (let ((node-count 1))
  (let ((node-list-length 0))
 (progn
   (setq node-rep-lst (node-rep-list (node-rep state 0 0 nil)))
   (setq result-node (astar-core node-rep-lst heuristic node-count duplicate-check node-list-length))
   (format t "~% A* SEARCH PATH IS - ~s" (reverse (fourth result-node)))
   
 )
))))
;-------------------------------------------------------------------------------
; MAIN IDA* FUNCTION IS CALLED WHICH INTERNALLY CALLS IDA*-CORE FUNCTION TO PROCESS IDA* SEARCH REQUEST
;-------------------------------------------------------------------------------
(defun idastar (state hu)
(let ((duplicate-check '()))
(let ((result-node '()))
 
  (progn
   (setq f-limit (h state hu))
   
   (loop 
     (unless (not(equal (first (first result-node)) *goal-state*)) (return-from idastar (reverse (fourth (first result-node))))) 
    
      (setq node-lst (node-rep-list (node-rep state (h state hu) 0 nil))) 
      (setq result-node (idastar-core node-lst hu f-limit 1000000000 duplicate-check))
      (setq f-limit (second result-node)) 
      
      )
      ))

))  


;------------------------------------------------------------------------------- 
;CONVERTS INPUT NODE TO THE REQUIRED NODE REPRESENTATION 
;-------------------------------------------------------------------------------
(defun node-rep (state h d op)
   (list state h d op)
)
;-------------------------------------------------------------------------------
;RETURNS NODE REPRESENTATION IN LIST FORM 
;-------------------------------------------------------------------------------
(defun node-rep-list (node)
	(list node)
)
;-------------------------------------------------------------------------------
;BFS FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun bfs-core (node-list node-count duplicate-check node-list-length)
;(format t "~% Node list : ~s" node-list)
	(let (cur-node tmp-node-list) 
	 (if (null node-list)
		NIL
		(progn
		  (setq cur-node (car node-list))
		  (setq tmp-node-list (cdr node-list))
		  (if (goalp cur-node) 
		    (progn
			(if ( > (list-length node-list) node-list-length)
             (progn
             (setq node-list-length (list-length node-list))
			 (format t "~% NODE-LIST-LENGTH : ~s" node-list-length)))	 
			
			(format t "~% TOTAL NODES VISITED : ~s" node-count)
			cur-node)
			(progn
			;IF CONDITIONS FAILS
			(setq duplicate-check (append duplicate-check ( list cur-node)))
			(setq tmp-node-list (append tmp-node-list (expand cur-node duplicate-check)))
			(setq node-count (+ node-count 1))
			(bfs-core tmp-node-list node-count duplicate-check node-list-length))
	  	  )  
	    )
     )
    )
)
;-------------------------------------------------------------------------------
;DFS FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun dfs-core (node-list node-count duplicate-check node-list-length)
	(let (cur-node tmp-node-list) 
	 (if (null node-list)
		NIL
		(progn
		  (setq cur-node (car node-list))
		  (setq tmp-node-list (cdr node-list))
		  (if (goalp cur-node) 
		    (progn
			(if ( > (list-length node-list) node-list-length)
             (progn
             (setq node-list-length (list-length node-list))
			 (format t "~% NODE-LIST-LENGTH : ~s" node-list-length)))	 
			(format t "~% TOTAL NODES VISITED : ~s" node-count)
			cur-node)
			(progn
			;IF CONDITIONS FAILS
			(setq duplicate-check (append duplicate-check ( list cur-node)))
			(setq tmp-node-list (append (expand cur-node duplicate-check) tmp-node-list))
			(setq node-count (+ node-count 1))
			(dfs-core tmp-node-list node-count duplicate-check node-list-length))
	  	  )  
	    )
     )
    )
)
;-------------------------------------------------------------------------------
;IDS FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun ids-core (node-list number node-count duplicate-check node-list-length)
	(let (cur-node tmp-node-list) 
	(if (null node-list)
		NIL
		(progn
		  (setq cur-node (car node-list))
		  (setq tmp-node-list (cdr node-list))
		  (if (goalp cur-node) 
		  (progn
		  (if ( > (list-length node-list) node-list-length)
             (progn
             (setq node-list-length (list-length node-list))
			 (format t "~% NODE-LIST-LENGTH : ~s" node-list-length)))	 
			(format t "~% TOTAL NODES VISITED : ~s" node-count)
			cur-node)
		  (progn
			(if ( >= number (third cur-node) )
			(progn
			(setq duplicate-check (append duplicate-check ( list cur-node)))
			(setq tmp-node-list (append tmp-node-list (expand cur-node duplicate-check)))
			(setq node-count (+ node-count 1))
			))
		   (ids-core tmp-node-list number node-count duplicate-check node-list-length)
		  )
	  	)
	)
    ))
)
;-------------------------------------------------------------------------------
;SORTS THE NODE LIST IN ASCENDING ORDER
;-------------------------------------------------------------------------------
(defun sort-node-list (node-list) (sort node-list (lambda (x y) (< (+ (second x) (third x)) (+ (second y) (third y))))))
(defun sort-node-list-greedy (node-list) (sort node-list (lambda (x y) (< (second x) (second y)) )))
;-------------------------------------------------------------------------------
;GREEDY FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun greedy-core (node-list heuristic node-count duplicate-check node-list-length)

	(let (cur-node tmp-node-list) 
	(if (null node-list)
		NIL
		(progn
		  ;;(format t "Here hi~% ~s" node-list)
		  (setq cur-node (car node-list))
		  (setq tmp-node-list (cdr node-list))
		  (if (goalp cur-node) 
		    (progn
			 (if ( > (list-length node-list) node-list-length)
             (progn
             (setq node-list-length (list-length node-list))
			 (format t "~% NODE-LIST-LENGTH : ~s" node-list-length)))
			(format t "~% TOTAL NODES VISITED : ~s" node-count)
			cur-node)
			(progn
			;false condition
			(setq duplicate-check (append duplicate-check ( list cur-node)))
			(setq tmp-node-list (append tmp-node-list (expand-h cur-node heuristic duplicate-check)))
		  (setq node-count (+ node-count 1))
		  (greedy-core (sort-node-list-greedy tmp-node-list) heuristic node-count duplicate-check node-list-length))
	  	)
	)
)))
;-------------------------------------------------------------------------------
;A* FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun astar-core (node-list heuristic node-count duplicate-check node-list-length)

	(let (cur-node tmp-node-list) 
	(if (null node-list)
		NIL
		(progn
		  ;;(format t "Here hi~% ~s" node-list)
		  (setq cur-node (car node-list))
		  (setq tmp-node-list (cdr node-list))
		  (if (goalp cur-node) 
		    (progn
			(if ( > (list-length node-list) node-list-length)
             (progn
             (setq node-list-length (list-length node-list))
			 (format t "~% NODE-LIST-LENGTH : ~s" node-list-length)))
			(format t "~% TOTAL NODES VISITED : ~s" node-count)
			cur-node)
			(progn
			;false condition
			(setq duplicate-check (append duplicate-check ( list cur-node)))
			(setq tmp-node-list (append tmp-node-list (expand-h cur-node heuristic duplicate-check)))
		  (setq node-count (+ node-count 1))
		  (astar-core (sort-node-list tmp-node-list) heuristic node-count duplicate-check node-list-length) )
	  	)
	)
)))
;-------------------------------------------------------------------------------
;IDA* FUNCTION TO PROCESS SEARCH TO REACH GOAL STATE
;-------------------------------------------------------------------------------
(defun idastar-core (node-list hu f-val new-f-limit duplicate-check)
  (let (cur-node tmp-node-list new-f-limit )) 
  (let ((cur-f-val 0))
  (if (null node-list)
    (progn
      
      (return-from idastar-core (append (list '()) (list new-f-limit))))
    (progn
      (setq cur-node (car node-list))
      (setq tmp-node-list (cdr node-list))
      
      (setq cur-f-val (+ (third cur-node) (third cur-node)))
      (setq new-f-limit (min (+  (third cur-node) (second cur-node)) new-f-limit))
      (if (goalp cur-node) 
        (progn 
		(format t "~% DEPTH OF RECURSION - ~s" (third cur-node)) 
         (append (list cur-node) (list new-f-limit)))
        (if (>= f-val (+  (third cur-node) (second cur-node)))
           (progn 
		   (setq duplicate-check (append duplicate-check (list cur-node)))
             (setq tmp-node-list (append tmp-node-list (expand-h cur-node hu duplicate-check) ))
             (dolist (node (expand-h cur-node hu duplicate-check))(if (< new-f-limit (+ (second node) (third node)))(setq new-f-limit (+ (second node) (third node)))))
             (idastar-core (sort-node-list tmp-node-list) hu f-val new-f-limit duplicate-check))
           (idastar-core (sort-node-list tmp-node-list) hu f-val new-f-limit duplicate-check)
    
  ))
  ))))
 

;-------------------------------------------------------------------------------
;PROCESSES THROUGH SUBTREES OF POSSIBLE STATES 
;-------------------------------------------------------------------------------
(defun expand (node duplicate-check)
(let ((res-list '()))
  (if (applicable 'up node)(if (not (equal (dupe (first (apply-op 'up node)) duplicate-check) T))(setf res-list (append res-list (list (apply-op 'up node))))))
  (if (applicable 'down node)(if (not (equal (dupe (first (apply-op 'down node)) duplicate-check) T))(setf res-list (append res-list (list (apply-op 'down node))))))
  (if (applicable 'left node)(if (not (equal (dupe (first (apply-op 'left node)) duplicate-check) T))(setf res-list (append res-list (list (apply-op 'left node))))))
  (if (applicable 'right node)(if (not (equal (dupe (first (apply-op 'right node)) duplicate-check) T))(setf res-list (append res-list (list (apply-op 'right node))))))
  (return-from expand res-list)
  )
)

			
(defun expand-h (node hu duplicate-check)
(let ((res-list '()))
(if (applicable 'up node)(if (not (equal (dupe (first (apply-op-h 'up node hu)) duplicate-check) T))(setf res-list (append res-list (list (apply-op-h 'up node hu))))))
(if (applicable 'down node)(if (not (equal (dupe (first (apply-op-h 'down node hu)) duplicate-check) T))(setf res-list (append res-list (list (apply-op-h 'down node hu))))))
(if (applicable 'left node)(if (not (equal (dupe (first (apply-op-h 'left node hu)) duplicate-check) T))(setf res-list (append res-list (list(apply-op-h 'left node hu))))))
(if (applicable 'right node)(if (not (equal (dupe (first (apply-op-h 'right node hu)) duplicate-check) T))(setf res-list (append res-list (list (apply-op-h 'right node hu))))))
(return-from expand-h res-list)
  )
)

;-------------------------------------------------------------------------------
; Look if op is applicable to state
;-------------------------------------------------------------------------------
(defun applicable (op node)
  (let* ((state (first node)) (blank (location 0 state)))
       (cond ((eq op 'up)
 	      (if (and (<= 0 blank) (>= 2 blank)) nil T))
	     ((eq op 'down)
	      (if (and (<= 6 blank) (>= 8 blank)) nil T))
	     ((eq op 'left)
	      (if (eq 0 (mod blank 3)) nil T))
	     ((eq op 'right)
	      (if (eq 2 (mod blank 3)) nil T))
	     (T 'failure))))

;-------------------------------------------------------------------------------
; Apply op to state
;-------------------------------------------------------------------------------
(defun apply-op (op node)
  (let* (new (state (first node)) (blank (location 0 state)))
   (setq *num-node* (+ *num-node* 1))
    (list (setq new 
            (cond ((eq op 'up)    (swap blank (- blank 3) state))
	          ((eq op 'down)  (swap blank (+ blank 3) state))
	          ((eq op 'left)  (swap blank (- blank 1) state))  
	          ((eq op 'right) (swap blank (+ blank 1) state))))
            (+ (huninf new)) 
            (+ (third node) 1)
            (cons op (fourth node)))))

(defun huninf (state) 
	'10000
)
(defun apply-op-h (op node hu)
  (let* (new (state (first node)) (blank (location 0 state)))
   (setq *num-node* (+ *num-node* 1))
   (list (setq new 
            (cond ((eq op 'up)    (swap blank (- blank 3) state))
	          ((eq op 'down)  (swap blank (+ blank 3) state))
	          ((eq op 'left)  (swap blank (- blank 1) state))  
	          ((eq op 'right) (swap blank (+ blank 1) state))))
            (+ (h new hu)) 
            (+ (third node) 1)
            (cons op (fourth node)))))

;-------------------------------------------------------------------------------
; find location of number in state (0 1 2 3 4 .. 8)
;-------------------------------------------------------------------------------
(defun location (number state)
  (let (loc)
     (dotimes (i *num-tile* loc)
        (if (= (nth i state) number)
           (setq loc i)
	   nil))))

;-------------------------------------------------------------------------------
; Swap values in two locations
;-------------------------------------------------------------------------------
(defun swap (loc1 loc2 lst)
  (let ((new nil))
     (dotimes (i *num-tile* (reverse new))
	(cond ((= i loc1) (setq new (cons (nth loc2 lst) new)))
	      ((= i loc2) (setq new (cons (nth loc1 lst) new)))
	      (T (setq new (cons (nth i lst) new)))))))

;-------------------------------------------------------------------------------
; CHECKS IF GOAL STATE IS REACHED
;-------------------------------------------------------------------------------
(defun goalp (node)
  (if (equal (first node) *goal-state*) T nil))

(defun dupe (state node-list) 
  (dolist (node node-list nil)
  ; (format t "~% NODELISTDUPE ~s"  node-list)
 ;(format t "~% STATE ~s"   state)
 ; (format t "~% NODEDUPE ~s" node)
  ;(format t "~% FIRST NODE~s" (first node))
     (if (equal state (first node)) 
	     (return-from dupe T))))
;-------------------------------------------------------------------------------
; HUERISTIC FUNCTION CALCULATION BASED ON MISSED TILES AND MANHATTAN DISTANCE
;-------------------------------------------------------------------------------
(defun h (state a)  
	(if (equal a 'h1)
	 (tiles_out_of_place state) 
	 (manhattan_distance state)
	)
)
(defun tiles_out_of_place(state)
  (let ((count 0))
     (loop for j from 0 to 8
         do (if (and (not(equal(nth j state)(nth j *goal-state*)))(not( equal (nth j state) 0)))(setq count(+ 1 count)))
     )
     (return-from tiles_out_of_place count)))

(defun manhattan_distance(state)
  (let ((total 0))
    (loop for i from 0 to 8
         do (if (and (not(equal (nth i state) (nth i *goal-state*)))(not( equal (nth i state) 0)))
		       (let ((finalindex (location (nth i state) *goal-state*)))
		       (let ((distance (+ (abs (- (mod finalindex 3) (mod i 3))) (abs (- (floor (/ finalindex 3)) (floor (/ i 3)))))))(setq total (+ total distance))))
            )
    )
        (return-from manhattan_distance total)
   )
)
