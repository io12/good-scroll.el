;;; good-scroll-bezier.el --- Bézier scrolling algorithm -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Homepage: https://github.com/io12/good-scroll.el

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

(require 'cl-lib)



;;;; General Bézier curve calculations

(defconst good-scroll-bezier--epsilon 0.0001
  "Epsilon for checking if floats are approximately equal.
The function `good-scroll-bezier--approx-eq-p' uses this.
Decreasing this `good-scroll-bezier--t-given-x' more accurate, but slower.")

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

(defun good-scroll-bezier--approx-eq-p (a b &optional epsilon)
  "Return whether the floating point values A and B are approximately equal.
The floats are considered approximately equal
if they differ by less than EPSILON,
or `good-scroll-bezier--epsilon' if EPSILON is nil."
  (< (abs (- a b))
     (or epsilon good-scroll-bezier--epsilon)))

(defun good-scroll-bezier--t-given-x (x x1 x2 &optional t-min t-max)
  "Estimate the t value of a cubic Bézier curve.
Given X (the output of the Bézier formula),
return the corresponding input value TT between T-MIN and T-MAX.
The Bézier curve is defined by the control points [0, X1, X2, 1].
The value of X must be in the interval [0,1]."
  ;; Use recursive binary search.
  ;; This works because the curve is always monotonically increasing.
  ;; Another approach is using Newton's method,
  ;; but that can be slow or get stuck when the slope is close to zero.
  (cl-assert (<= 0.0 x 1.0))
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



;;;; Integration with `good-scroll'

(defgroup good-scroll-bezier nil
  "Good-scroll Bézier scrolling algorithm"
  :group 'good-scroll)

;;;;; Bézier curve control points

(defvar good-scroll-bezier--x1 nil
  "X coordinate of first control point.")
(defvar good-scroll-bezier--y1 nil
  "Y coordinate of first control point.")
(defvar good-scroll-bezier--x2 0.6
  "X coordinate of second control point.")
(defvar good-scroll-bezier--y2 1.0
  "Y coordinate of second control point.")

;;;;; Information about previous scroll event

(defvar good-scroll-bezier--prev-time 0.0
  "Time of the last received scroll event.
This is used for checking for new scroll events.")

(defvar good-scroll-bezier--prev-direction 0
  "Direction of the last received scroll event.
This is used for checking if the direction changed in a scroll event.")

;;;;; Bézier curve visualization options

(defcustom good-scroll-bezier-image-display nil
  "When non-nil, display an animation of the current Bézier curve.
Because of garbage collector pauses, this is very slow."
  :group 'good-scroll-bezier
  :type 'boolean)

(defcustom good-scroll-bezier-image-size 50
  "Size of Bézier curve image to draw.
When the variable `good-scroll-bezier-image-display' is non-nil,
this is the side length of the image in pixels.
Larger values may have significantly worse performance."
  :group 'good-scroll-bezier
  :type 'integer)

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

(defun good-scroll-bezier ()
  "Bézier scrolling algorithm.
Return the next position in pixel lines.
Update the internal Bézier curve on new scroll events."
  (let* ((time (float-time))
         (elapsed-time (- time good-scroll-start-time))
         (prev-elapsed-time (- time good-scroll-bezier--prev-time))
         (fraction-done (min 1.0 (/ elapsed-time good-scroll-duration)))
         (prev-fraction-done (min 1.0 (/ prev-elapsed-time good-scroll-duration)))
         (direction-changed-p (<= (* good-scroll-direction
                                     good-scroll-bezier--prev-direction)
                                  0)))

    ;; Update Bézier curve visualization
    (when good-scroll-bezier-image-display
      (let ((window (selected-window)))
        (good-scroll-bezier-image-display good-scroll-bezier-image-size
                                          good-scroll-bezier-image-size
                                          fraction-done)
        (select-window window)))

    ;; New scroll event received?
    (when (/= good-scroll-bezier--prev-time good-scroll-start-time)
      ;; Got a new scroll event, so update the Bézier curve.
      (if direction-changed-p
          ;; Zero velocity if direction changed
          (good-scroll-bezier--set-points 0.0)
        ;; Maintain velocity if direction stayed the same
        (good-scroll-bezier--update prev-fraction-done)))

    ;; Mark this scroll event as received
    (setq good-scroll-bezier--prev-time good-scroll-start-time)
    (setq good-scroll-bezier--prev-direction good-scroll-direction)

    (good-scroll-bezier--position fraction-done)))



;;;;; Visualize image of Bézier curve in a separate window

(defun good-scroll-bezier--bitmap (width height fraction-done)
  "Return a bitmap of the current Bézier curve.
Return a vector of vectors of integers representing the bitmap.
Each integer is a pixel, and is zero for black and one for white.
The dimensions of the bitmap are given by WIDTH and HEIGHT.
Draw a vertical line at FRACTION-DONE."
  (let ((bitmap (make-vector height nil)))
    ;; Initialize rows
    (dotimes (y height)
      (aset bitmap y (make-vector width 0)))
    ;; Plot progress line
    (let ((x (truncate (* fraction-done 0.99 width))))
      (dotimes (y height)
        (aset (aref bitmap y) x 1)))
    ;; Plot control points
    (let ((x1 (truncate (* good-scroll-bezier--x1 0.99 width)))
          (x2 (truncate (* good-scroll-bezier--x2 0.99 width)))
          (y1 (truncate (* good-scroll-bezier--y1 0.99 height)))
          (y2 (truncate (* good-scroll-bezier--y2 0.99 height))))
      (aset (aref bitmap y1) x1 1)
      (aset (aref bitmap y2) x2 1))
    ;; Set a bit in each column (as part of the curve)
    (dotimes (x width)
      (let* ((tt (good-scroll-bezier--t-given-x (/ (float x) width)
                                                good-scroll-bezier--x1
                                                good-scroll-bezier--x2))
             (y-frac (good-scroll-bezier--calc tt
                                               good-scroll-bezier--y1
                                               good-scroll-bezier--y2))
             (y (truncate (* y-frac height))))
        (aset (aref bitmap y) x 1)))
    bitmap))

(defun good-scroll-bezier--image (width height fraction-done)
  "Return a string with a PBM image of the current Bézier curve.
The dimensions of the image are given by WIDTH and HEIGHT.
Draw a vertical line at FRACTION-DONE."
  (format "P1\n# good-scroll test bitmap\n%d %d\n%s"
          width
          height
          (mapconcat (lambda (row) (mapconcat #'number-to-string row " "))
                     (reverse (good-scroll-bezier--bitmap width
                                                          height
                                                          fraction-done))
                     "\n")))

(defun good-scroll-bezier-image-display (width height fraction-done)
  "Display an image of the current Bézier curve.
The dimensions of the image are given by WIDTH and HEIGHT.
Draw a vertical line at FRACTION-DONE."
  (cl-assert (<= 0.0 fraction-done 1.0))
  (let ((buffer (get-buffer-create " *good-scroll-bezier-image-display*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert-image
       (create-image (good-scroll-bezier--image width height fraction-done)
                     nil
                     t
                     :scale 1)))
    (pop-to-buffer buffer)))



(provide 'good-scroll-bezier)

;;; good-scroll-bezier.el ends here
