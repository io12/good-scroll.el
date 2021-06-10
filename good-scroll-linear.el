;;; good-scroll-linear.el --- Linear scrolling algorithm -*- lexical-binding: t; -*-

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

;; This library implements an algorithm for `good-scroll'
;; that linearly interpolates the position in pixel lines.
;; It's older, simpler, and feels less smooth than `good-scroll-bezier'.
;; Set `good-scroll-algorithm' to `good-scroll-linear' to enable.

;;; Code:

(defun good-scroll-linear (fraction-done)
  "Linear scrolling algorithm.
Return the next position in pixel lines when the scroll is FRACTION-DONE done.
This works by linearly interpolating position."
  (round (- (* fraction-done
               (+ good-scroll-traveled
                  good-scroll-destination))
            good-scroll-traveled)))

(ert-deftest good-scroll-linear ()
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

(provide 'good-scroll-linear)

;;; good-scroll-linear.el ends here
