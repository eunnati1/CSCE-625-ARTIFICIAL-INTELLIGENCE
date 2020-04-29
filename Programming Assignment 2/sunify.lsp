;  sunify.lsp            Gordon S. Novak Jr.             10 Nov 92
;			http://www.cs.utexas.edu/users/novak

; Simple unification program

;(in-package :user)

; Get a list of the variables in an expression
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

; Examples
; (unify '(p x) '(p (a)))
; (unify '(p (a)) '(p x))
; (unify '(p x (g x) (g (b))) '(p (f y) z y))
; (unify '(p (g x) (h w) w) '(p y (h y) (g (a))))
; (unify '(p (f x) (g (f (a))) x) '(p y (g y) (b))) ; = nil
; (unify '(p x) '(p (a) (b)))                       ; = nil
; (unify '(p x (f x)) '(p (f y) y))                 ; = nil