;;; good-scroll-test.el --- Unit testing for good-scroll -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Homepage: https://github.com/io12/good-scroll.el
;; Package-Requires: ((emacs "27.1"))

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

;; This library contains ERT unit tests for good-scroll.
;; They can be run with `ert-run-tests-interactively'.

;;; Code:

(require 'good-scroll)

(require 'ert)



;;; good-scroll-linear.el

(ert-deftest good-scroll-test-linear ()
  (with-temp-buffer
    (cl-flet ((test-case
               (traveled destination zero half one)
               (set (make-local-variable 'good-scroll-traveled) traveled)
               (set (make-local-variable 'good-scroll-destination) destination)
               (should (= (good-scroll-linear 0.0) zero))
               (should (= (good-scroll-linear 0.5) half))
               (should (= (good-scroll-linear 1.0) one))))
      (test-case 0 10 0 5 10)
      (test-case 0 -10 0 -5 -10)
      (test-case 10 20 -10 5 20)
      (test-case -10 20 10 15 20)
      (test-case 10 -20 -10 -15 -20))))



;;; good-scroll-bezier.el

(ert-deftest good-scroll-test-bezier-t-given-x ()
  (with-temp-buffer
    (cl-flet ((test-case
               (x x1 x2)
               (let* ((tt (good-scroll-bezier--t-given-x x x1 x2))
                      (x- (good-scroll-bezier--calc tt x1 x2)))
                 (should (good-scroll-bezier--approx-eq-p x x-)))))
      (test-case 0.0 0.0 0.0)
      (test-case 0.5 0.0 0.0)
      (test-case 0.0 0.1 3.1)
      (test-case 1.0 2.0 3.0)
      (test-case 0.0 -0.1 3.1)
      (test-case 0.0 0.1 -3.1)
      (test-case 1.0 -2.0 -3.0)
      (test-case 1.0 2.0 -3.0)
      (test-case 0.5 2.0 3.0)
      (test-case 1.0 -2.0 3.0))))

(ert-deftest good-scroll-test-bezier-maintain-velocity ()
  (with-temp-buffer
    (cl-flet ((test-case
               (velocity duration traveled destination epsilon half)
               (set (make-local-variable 'good-scroll-duration) duration)
               (set (make-local-variable 'good-scroll-traveled) traveled)
               (set (make-local-variable 'good-scroll-destination) destination)
               (good-scroll-bezier--set-points velocity)
               (should (good-scroll-bezier--approx-eq-p
                        (good-scroll-bezier--velocity-at 0.0) velocity epsilon))
               (should (good-scroll-bezier--approx-eq-p
                        (good-scroll-bezier--velocity-at 0.5) half epsilon))
               (should (good-scroll-bezier--approx-eq-p
                        (good-scroll-bezier--velocity-at 1.0) 0.0 epsilon))))
      (test-case 0.0 0.1 0 1 0.01 14.2934)
      (test-case 0.0 1.0 0 1 0.01 1.4293)
      (test-case 0.0 10.0 0 1 0.01 0.1429)
      (test-case 1.0 0.1 0 1 0.01 14.0608)
      (test-case 1.0 1.0 0 1 0.01 1.1677)
      (test-case 1.0 10.0 0 1 0.1 0.1406)
      (test-case 1234.56 0.1 50 20 1000.0 1.0)
      (test-case 1234.56 1.0 50 20 1000.0 1.0)
      (test-case 1234.56 10.0 50 20 1000.0 1.0))))

(defun good-scroll-test-bezier-bitmap (width height fraction-done)
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
      (dotimes (y width)
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

(defun good-scroll-test-bezier-image (width height fraction-done)
  "Return a string with a PBM image of the current Bézier curve.
The dimensions of the image are given by WIDTH and HEIGHT.
Draw a vertical line at FRACTION-DONE."
  (format "P1\n# good-scroll test bitmap\n%d %d\n%s"
          width
          height
          (mapconcat (lambda (row) (mapconcat #'number-to-string row " "))
                     (reverse (good-scroll-test-bezier-bitmap width
                                                              height
                                                              fraction-done))
                     "\n")))

(defun good-scroll-test-bezier-image-display (width height fraction-done)
  "Display an image of the current Bézier curve.
The dimensions of the image are given by WIDTH and HEIGHT.
Draw a vertical line at FRACTION-DONE."
  (cl-assert (<= 0.0 fraction-done 1.0))
  (let ((buffer (get-buffer-create " *good-scroll-test-bezier-image-display*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert-image
       (create-image (good-scroll-test-bezier-image width height fraction-done)
                     nil
                     t)))
    (pop-to-buffer buffer)))



(provide 'good-scroll-test)

;;; good-scroll-test.el ends here
