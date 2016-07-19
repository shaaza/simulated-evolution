;;; Testing framework:
(load "lisp-unit.lisp")

;;;; Simulated Evolution

;;; Topology

(defparameter *width* 100)
(defparameter *height* 50)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)



;;; Plants

;; Creation

; plant hash-table
(defparameter *plants* (make-hash-table :test #'equal))

; random-plant
(defun random-plant (left top width height)
                    (let ((pos (cons (+ left (random width)) (+ top (random height)))))
                          (setf (gethash pos *plants*) t)))
; add-plant
(defun add-plants ()
                  (apply #'random-plant *jungle*)
                  (random-plant 0 0 *width* *height*))

;;; Animals

;; Animal Structure

(defstruct animal x y energy dir genes)

;; Creation

(defparameter *animals* 
              (list (make-animal :x          (ash *width* -1)
                                 :y          (ash *height* -1)
                                 :energy     1000
                                 :dir        0
                                 :genes      (loop repeat 8 collect (1+ (random 10))))))

;; Motion 

(defun move (animal)
            (let ((dir (animal-dir animal))
                  (x   (animal-x   animal))
                  (y   (animal-y   animal)))
                 
                  (setf (animal-x animal)
                        (mod (+ x
                                (cond ((and (>= dir 2) (< dir 5)) 1)
                                      ((or (= dir 1) (= dir 5)) 0)
                                      (t -1)))
                             *width*))
                  
                  (setf (animal-y animal)
                        (mod (+ y
                                (cond ((and (>= dir 0) (< dir 3)) 1)
                                      ((or (= dir 7) (= dir 3)) 0)
                                      (t -1)))
                             *height*))
                  (decf (animal-energy animal))))

;; Turning

(defun turn (animal)
            (let ((x (random (apply #'+ (animal-genes animal)))))
                 (labels ((angle (genes x)
                                 (let ((xnu (- x (car genes))))
                                      (if (< xnu 0)
                                      0
                                      (1+ (angle (cdr genes) xnu))))))
                         (setf (animal-dir animal) 
                               (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                                    8)))))


;; Eating

(defun eat (animal)
           (let ((pos (cons (animal-x animal) (animal-y animal))))
                (when (gethash pos *plants*)
                      (incf (animal-energy animal) *plant-energy*)
                      (remhash pos *plants*))))

;; Reproduction
; [config] Minimum energy required for reproduction

(defparameter *reproduction-energy* 200)

; Reproduce: 1. Find energy of animal. | 2. If greater than *reproduction-energy*, execute reproduction code. | 2.1 Halve energy | 2.2 Copy animal structure and genes | 
;            2.3 Find a random mutation value. | 2.4 Mutate a single one of the 8 genes of the child slightly and randomly. | 2.5 Push new animal into *animals*

(defun reproduce (animal)
                 (let ((e (animal-energy animal)))
                      (when (>= e *reproduction-energy*)
                            (setf (animal-energy animal) (ash e -1))
                            (let ((animal-nu (copy-structure animal))
                                  (genes (copy-list (animal-genes animal)))
                                  (mutation (random 8)))
                                 (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
                                 (setf (animal-genes animal-nu) genes)
                                 (push animal-nu *animals*)))))

;;; Simulation

;; Update the World: 1. Remove all dead animals. | 2. Make all animals turn, move, eat and reproduce. | 3. Add plants.

(defun update-world ()
                    (setf *animals* (remove-if (lambda (animal) 
                                                       (<= (animal-energy animal) 0))
                                               *animals*))
                    
                    (mapc (lambda (animal)
                                  (turn animal)
                                  (move animal)
                                  (eat animal)
                                  (reproduce animal))
                          *animals*)
                     
                    (add-plants))

;; Drawing the World: 1. Iterate over Y, i.e. from 0 to *height*. | 1.1 For each iteration of Y, iterate over X to *width*. | 1.1.1 For each iteration of X, print M if an animal exists, print * if a plant exists and a space if neither are present.

(defun draw-world () 
                  (loop for y 
                        below *height*
                        do (progn (fresh-line)
                                  (princ "|")
                                  (loop for x
                                        below *width*
                                        do (princ (cond ((some (lambda (animal) 
                                                                       (and (= (animal-x animal) x) (= (animal-y animal) y)))
                                                               *animals*)
                                                         #\M)
                                                         ((gethash (cons x y) *plants*) #\*)
                                                         (t #\space))))
                                  (princ "|"))))
;; User Interface: 1. Draw the world. | 2. Wait for commands. | 2.1 If command is quit, return NIL. | 2.2 Otherwise, parse-integer with the command with junk allowed. | 2.2.1 If command is an integer n, it must update world n times, and print a dot (.) at the end of each 1000 iterations. | 2.2.2 If it is not an integer, return NIL. Update-world just once.

(defun evolution ()
                 (draw-world)
                 (fresh-line)
                 (let ((str (read-line)))
                      (cond ((equal str "quit") ())
                            (t (let ((x (parse-integer str :junk-allowed t)))
                                     (if x
                                         (loop for i
                                               below x
                                               do (update-world)
                                               if (zerop (mod i 1000))
                                               do (princ #\.))
                                          (update-world))
                                      (evolution)))))) 
