
;; Roughtly approximates Interlisp LOOPS active values
;; An active value has subscribers that get called on value change
(defclass active-value ()
  ;; maybe subscribers are Class level, think about this later
  ((value :initarg :value :accessor value :initform nil)
   (actions :initarg :actions :accessor actions :initform (list))))

;; Add a subscriber to the active value
(defmethod add-action ((av active-value) fn)
           (setf (actions av) ;; TODO Graham queue later
                 `(,@(actions av) ,fn)))

;; Override setting modifier for active-values
(defmethod (setf value) (new-value (object active-value))
           (dolist (fn (actions object))
             (funcall fn new-value)))
