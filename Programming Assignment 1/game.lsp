
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; GAME PLAYING
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
; global variables: number of tiles
;-------------------------------------------------------------------------------
(defvar *num-tile* 9)
(defvar *num-node* 0)

;-------------------------------------------------------------------------------
; MIN-MAX
;-------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; RECURSIVELY CALLS MINIMAX VALUE FUNCTION AND UPDATES VALUE OF EACH CURRENT NODE
(defun Minimax-Decision (game) 
 (let (cur-node curr-val child-val h count)  ; h decides if value should be max or minimum. 1= max-value , -1 = min-value
      (setq cur-node game)
      (setq h 1)    	 ; starts with max function
      (loop while (not(atom cur-node))do  ; iterate till we reach the leaf node
	   (progn
	      (setq curr-val (Minimax-Value cur-node h))   ; calculate minimax value of each present node by calling the minimav-value function
	      (setq child-val (+ 10 curr-val))  ; ; initially keep child value not equal to curr-val
	      (setq count 0)   ; keep a counter to keep track of path traversed
	      (loop
	       while (not(equal child-val curr-val)) do 
	        (progn
	           (setq child-val (Minimax-Value (nth count cur-node) (* h -1)))  ; call min function for child (min-max alternating)
	           (setq count (+ count 1))))
	   (setq cur-node  (nth (- count 1) cur-node))
	   (setq curr-val child-val)
	   (format t " PATH TAKEN : ~s" count)
	   (setq h (* h -1))    ; alternate between minimum and maximum value.
	   )
	  )
		
    )
 )
 
; Returns the min/max value for the called state from Minimax-Decision function
(defun Minimax-Value(state game)
   (if (goal state)          ; if goal state reached, return value
       (return-from Minimax-Value state))

 (if (= game 1)  ; if max node called then
  (let ((maximum_node -10000000000))   ; initialise alpha to -infinity
     (loop for i in state
    	 do (setq maximum_node (max maximum_node (Minimax-Value i -1))) ; Recursivley call function 
	 )  
	 (return-from Minimax-Value maximum_node)
  )
 )
 
  (if (= game -1) ; if min node called then
    (let ((minimum_node 1000000000000)) ; initialise beta to +infinity
    	(loop for j in state 
		   do (setq minimum_node (min minimum_node (Minimax-Value j 1))) ; Recursivley call function 
		) 
		(return-from Minimax-Value minimum_node) 
	)
  )
)

;-------------------------------------------------------------------------------
; ALPHA-BETA Pruning
;-------------------------------------------------------------------------------
;------------------------------------------------------------------------------------------------------------------------------
(defun Minimax-DecisionAB (game) 
 (let (cur-node curr-val child-val g count) ;g decides if value should be max or minimum. 1= max-value , -1 = min-value
      (setq cur-node game)
      (setq g 1)    	  ; starts with max function
      (loop while (not(atom cur-node)) 	do  ; iterate till we reach the leaf node
	   (progn
	      (setq curr-val (Minimax-ValueAB cur-node g)) ; calculate minimax value of each present node by calling the minimax-value function
	      (setq count 0)
		  (setq child-val (+ 10 curr-val))  ; ; initially keep child value not equal to curr-val
	      (loop while (not(equal child-val curr-val)) do
	        (progn
	           (setq child-val (Minimax-ValueAB (nth count cur-node) (* -1 g))) ;call min function for child (min-max alternating)
	           (setq count (+ count 1))
	        )
	   
	      )
	   (setq cur-node  (nth (- count 1) cur-node))
	   (setq curr-val child-val)
	   (format t " PATH TAKEN WITH ALPHA BETA PRUNING : ~s" count)
	   (setq g (* g -1)) ; alternate between minimum and maximum value.
	   )
	  )
		
    )
 )
	

; Returns the min/max value for the called state from Minimax-Decision function
(defun Minimax-ValueAB(state g)

    (if (= g 1) (return-from Minimax-ValueAB (max-value state -1000000 1000000)))
    (if (= g -1) (return-from Minimax-ValueAB (min-value state -1000000 1000000)))
)

;MAX Value FUNCTION
(defun max-value(state a b)
   (if (goal state) (return-from max-value state)) ; if goal state reached, return value
  
     (let ((v -100000))(loop for i in state do 
          (progn
          (setq v (max a (min-value i a b)))
		  (if (>= v b) 
          (progn
          (format t "~%  MAX CUT AFTER = ~s" i "in subtree=~s" state)
           (return-from max-value v)))
      (setq a (max a v))))
     (return-from max-value v))
)
;MIN Value FUNCTION
(defun min-value(state a b)
       (if (goal state)(return-from min-value state)) ; if goal state reached, return value
  
        (let ((v 100000))(loop for i in state do 
        (progn
        (setq v (min b (max-value i a b)))

        (if (<= v a)
        (progn
        (format t "~%  MIN CUT AFTER =~s" i "in subtree=~s" state)
         (return-from min-value v)))
        (setq b (min b v))))
(return-from min-value v)))
;-------------------------------------------------------------------------------
; CHECKS IF GOAL STATE IS REACHED
;-------------------------------------------------------------------------------

(defun goal(state)
  (if (atom state) T nil))

;-------------------------------------------------------------------------------
;PROCESSES THROUGH SUBTREES OF POSSIBLE STATES 
;-------------------------------------------------------------------------------
(defun expand (node)
	(if (atom node) 
	    NIL
	   (car node)
	)
)

;-------------------------------------------------------------------------------
; find location of number in state (0 1 2 3 4 .. 8)
;-------------------------------------------------------------------------------
(defun location (number state)
  (let (loc)
     (dotimes (i *num-tile* loc)
        (if (= (nth i state) number)
           (setq loc i)
	   nil))))
