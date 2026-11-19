(in-package #:guim)

(defun clamp (val min max)
  "Ensure returned value is clamped between min and max."
  (cond ((and min (< val min)) min)
        ((and max (> val max)) max)
        (t val)))
