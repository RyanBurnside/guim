;;;; guim.lisp

(in-package #:guim)

;;; Package level defaults on offer to user
(defparameter *default-button-width* 64)
(defparameter *default-button-height* 24)

;;; TODO
;;; Add a container widget that watches parent active values and stores children
;;; This splits rendering into a method and also updating into amethod
(defclass guim-vm ()
  ((widgets :accessor widgets :initarg :widgets :initform nil)))

(defclass point ()
  ((x :accessor x :initarg :x :initform 0f0)
   (y :accessor y :initarg :y :initform 0f0)))

(defgeneric move (thing x y))

(defmethod move ((p point) x y)
  (setf (x p) x
        (y p) y)
  p)

(defclass area ()
  ((width :accessor width :initarg :width :initform 1f0)
   (height :accessor height :initarg :height :initform 1f0)
   (max-width :accessor max-width :initarg :max-width :initform nil)
   (max-height :accessor max-height :initarg :max-height :initform nil)
   (min-width :accessor min-width :initarg :min-width :initform nil)
   (min-height :accessor min-height :initarg :min-height :initform nil)))

;; Powerful optional limiting on widget dimensions
(defmethod (setf width) (new-value (a area))
  (let ((min-w (min-width a))
        (max-w (max-width a)))
    (setf (slot-value a 'width)
          (cond ((and min-w (< new-value min-w)) min-w)
                ((and max-w (> new-value max-w)) max-w)
                (t new-value)))))

(defmethod (setf height) (new-value (a area))
  (let ((min-h (min-height a))
        (max-h (max-height a)))
    (setf (slot-value a 'height)
          (cond ((and min-h (< new-value min-h)) min-h)
                ((and max-h (> new-value max-h)) max-h)
                (t new-value)))))

;; Ensures width and height conform
(defmethod initialize-instance :after ((a area) &key)
  (setf (width a) (width a)
        (height a) (height a)))

(defgeneric resize (thing width height))

(defmethod resize ((a area) width height)
  (setf (width a) width
        (height a) height))

(defclass region (point area)())

;;; GUI Elements
(defclass interactable (region)
  ((active :accessor active :initarg :active :initform t)
   (focused :accessor focused :initarg :focused :initform nil)
   (fn :accessor fn :initarg :fn :initform nil)))

(defclass drawable (region)
  ((style :accessor style :initarg :style :initform nil)))

(defgeneric draw (drawable &optional backend)
  (:documentation "Render a drawable in the current style (defined by drawable) using backend."))

(defclass ui-widget (drawable interactable) ())

;; It important for the user to use ui-labels rather than just a string.
;; TODO put some safeguards around expectect ui-labels
(defclass ui-label (ui-widget)
  ((text :accessor text
         :initarg :text
         :initform ""
         :type string)))

(defun make-ui-label (string &rest initargs)
  "Create a ui-label converting input to a string."
  (let ((temp (apply #'make-instance 'ui-label :text string initargs)))
    (unless (stringp (text temp))
      (setf (text temp) (format nil "~a" string)))
    temp))

(defclass ui-button (ui-widget)
  ((label :accessor label
          :initarg :label
          :type ui-label))
  (:default-initargs :label (make-ui-label "")))

(defclass ui-window (ui-widget)
  ((label :accessor label
          :initarg :label
          :type ui-label))
  (:default-initargs :label (make-ui-label "")))


;;; GUI Container

(defclass ui-container (ui-widget)
  ((contents :accessor contents
             :initarg :contents
             :initform nil)))

(defclass ui-grid-container (ui-container) ())

(defclass ui-freeform-container (ui-container) ())

(defclass ui-framesplit-container (ui-container) ())
