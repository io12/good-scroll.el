;;; good-scroll-bezier.el --- Bézier scrolling algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This implements a scrolling algorithm for `good-scroll'
;; based on Bézier curves.
;; This is newer and feels smoother than `good-scroll-linear',
;; but is more complicated.
;; Set `good-scroll-algorithm' to `good-scroll-bezier-position' to enable.

;;; Code:



;;;; General Bézier curve calculations

(defvar good-scroll-bezier--epsilon 0.0001
  "Epsilon for checking if floats are approximately equal.
The function `good-scroll-bezier--approx-eq-p' uses this.")

(defun good-scroll-bezier--calc (tt p1 p2)
  "Compute the cubic Bézier polynomial at TT with control points [0, P1, P2, 1].
The calculation is one-dimensional,
meaning TT, P1, and P2 are numbers instead of vectors.
Two-dimensional calculations can be done by evaluating this function twice,
once for each of the X and Y values of the control points P1 and P2.
More information can be found at the URL
`https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Cubic_B%C3%A9zier_curves'."
  (+ (* 3 (expt (- 1 tt) 2) tt p1)
     (* 3 (- 1 tt) (expt tt 2) p2)
     (expt tt 3)))

(defun good-scroll-bezier--deriv (tt p1 p2)
  "Compute the derivative of `good-scroll-bezier--calc' with respect to TT.
Compute the derivative of the cubic Bézier polynomial
defined by the control points [0, P1, P2, 1]."
  (+ (* 3 (expt (- 1 tt) 2) p1)
     (* 6 (- 1 tt) tt (- p2 p1))
     (* 3 (expt tt 2) (- 1 p2))))

(defun good-scroll-bezier--approx-eq-p (a b)
  "Return whether the floating point values A and B are approximately equal.
The floats are considered approximately equal
if they differ by less than `good-scroll-bezier--epsilon'."
  (< (abs (- a b))
     good-scroll-bezier--epsilon))

(defun good-scroll-bezier--t-given-x (x x1 x2 &optional t-min t-max)
  "Estimate the t value of a cubic Bézier curve.
Given X (the output of the Bézier formula),
return the corresponding input value TT between T-MIN and T-MAX.
The Bézier curve is defined by the control points [0, X1, X2, 1]."
  ;; Use recursive binary search.
  ;; This works because the curve is always monotonically increasing.
  ;; Another approach is using Newton's method,
  ;; but that can be slow or get stuck when the slope is close to zero.
  (let* (
         (t-min (or t-min 0.0))
         (t-max (or t-max 1.0))
         (t-mid (/ (+ t-min t-max) 2))
         (x- (good-scroll-bezier--calc t-mid x1 x2)))
    (cond
     ;; Equal
     ((good-scroll-bezier--approx-eq-p x- x)
      ;; Return the approximation
      t-mid)
     ;; Less than
     ((< x- x)
      ;; Try upper half
      (good-scroll-bezier--t-given-x x x1 x2 t-mid t-max))
     ;; Greater than
     (t
      ;; Try lower half
      (good-scroll-bezier--t-given-x x x1 x2 t-min t-mid)))))



;;; Integration with `good-scroll'

(defvar good-scroll-bezier--x1 nil
  "X coordinate of first control point.")
(defvar good-scroll-bezier--y1 nil
  "Y coordinate of first control point.")
(defvar good-scroll-bezier--x2 0.6
  "X coordinate of second control point.")
(defvar good-scroll-bezier--y2 1.0
  "Y coordinate of second control point.")

(defvar good-scroll-bezier--prev-time nil
  "Time of the last received scroll event.
This is used for checking for new scroll events.")

(defun good-scroll-bezier--set-points (velocity)
  "Update the control points.
Modify the control points such that `(good-scroll-bezier--velocity-at 0.0)'
will return approximately VELOCITY."
  (let* (
         ;; Total distance the scroll will have traveled when it finishes
         (total-distance (+ good-scroll-traveled good-scroll-destination))
         ;; Reconstruct dy/dx from velocity by reversing operations
         ;; at the end of `good-scroll-bezier--velocity-at'.
         (dy/dx (* velocity (/ good-scroll-duration total-distance)))
         ;; This is similar to `(abs dy/dx)',
         ;; but if `dy/dx' is zero then `normalization' is 1.
         (normalization (sqrt (+ 1.0 (expt dy/dx 2))))
         (normalization (/ 0.25 normalization))
         ;; The goal is to choose values `x' and `y'
         ;; such that `(/ y x)' equals `dy/dx'.
         ;; TODO: Talk about normalization
         (x normalization)
         (y (* dy/dx normalization)))
    ;; The first control point should determine the dy/dx when t is zero,
    ;; and therefore preserve the velocity.
    (setq good-scroll-bezier--x1 x
          good-scroll-bezier--y1 y)))

(defun good-scroll-bezier--velocity-at (fraction-done)
  "Return the current velocity of the scrolling in pixel-lines per second.
The argument FRACTION-DONE is a number between 0.0 and 1.0,
indicating completion progress."
  (let* (
         (tt (good-scroll-bezier--t-given-x fraction-done
                                            good-scroll-bezier--x1
                                            good-scroll-bezier--x2))
         (dx/dt (good-scroll-bezier--deriv tt
                                           good-scroll-bezier--x1
                                           good-scroll-bezier--x2))
         (dy/dt (good-scroll-bezier--deriv tt
                                           good-scroll-bezier--y1
                                           good-scroll-bezier--y2))
         ;; Slope of line tangent to the Bézier curve
         (dy/dx (/ dy/dt dx/dt)) ; TODO make sure dx/dt != 0
         ;; Total distance the scroll will have traveled when it finishes
         (total-distance (+ good-scroll-traveled good-scroll-destination)))
    ;; The x-axis of the Bézier curve represents time
    ;; and the y-axis represents position.
    ;; However, the domain and range are both [0, 1],
    ;; so we need to scale the curve by the total distance and duration.
    ;; The slope dy/dx represents what the speed would be
    ;; if the distance and duration were both 1.
    ;; So we need to scale the slope with the distance and duration.
    (* dy/dx (/ total-distance good-scroll-duration))))

(defun good-scroll-bezier--position (fraction-done)
  "Return the current position of the scroll in pixel-lines.
The argument FRACTION-DONE is a number between 0.0 and 1.0,
indicating time-based completion progress."
  (let* (
         (tt (good-scroll-bezier--t-given-x fraction-done
                                            good-scroll-bezier--x1
                                            good-scroll-bezier--x2))
         ;; Pixel-based scroll progress
         (progress (good-scroll-bezier--calc tt
                                             good-scroll-bezier--y1
                                             good-scroll-bezier--y2)))
    (round (- (* progress (+ good-scroll-traveled
                             good-scroll-destination))
              good-scroll-traveled))))

(defun good-scroll-bezier--update (fraction-done)
  "Update the Bézier curve's control points.
Modify the control points such that velocity is preserved.
Assume the scroll's progress is FRACTION-DONE."
  ;; Try to get the velocity,
  ;; or use zero if the first control point is uninitialized.
  (let ((velocity (if good-scroll-bezier--x1
                      (good-scroll-bezier--velocity-at fraction-done)
                    0.0)))
    ;; Actually update the control points
    (good-scroll-bezier--set-points velocity)))
    ;; TODO: Use unit tests for this
    ;; (let ((v (good-scroll-bezier--velocity-at 0.0)))
    ;;   (unless (good-scroll-bezier--approx-eq-p velocity v)
    ;;     (message (format "%s %s" velocity v))))))

(defun good-scroll-bezier (fraction-done)
  "Bézier scrolling algorithm.
Return the next position in pixel lines when the scroll is FRACTION-DONE done.
Update the internal Bézier curve on new scroll events."
  ;; New scroll event received?
  (unless (equal good-scroll-bezier--prev-time
                 good-scroll-start-time)
    ;; Got a new scroll event, so update the Bézier curve.
    (good-scroll-bezier--update fraction-done)
    ;; Mark this scroll event as received
    (setq good-scroll-bezier--prev-time
          good-scroll-start-time))

  (good-scroll-bezier--position fraction-done))



(provide 'good-scroll-bezier)

;;; good-scroll-bezier.el ends here
