;;; good-scroll.el --- Good pixel line scrolling -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; Author: Benjamin Levy <blevy@protonmail.com>
;; Version: 0.3.0
;; Description: Attempt at good pixel-based smooth scrolling in Emacs
;; Homepage: https://github.com/io12/good-scroll.el
;; Package-Requires: ((emacs "24.4"))

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

;; This package implements smooth scrolling by pixel lines. It attempts to
;; improve upon `pixel-scroll-mode' by adding variable speed.

;;; Code:

(require 'cl-lib)

(require 'good-scroll-bezier)
(require 'good-scroll-linear)

(defgroup good-scroll nil
  "Good pixel line scrolling"
  :group 'scrolling)

(defcustom good-scroll-render-rate (/ 1.0 30.0)
  "Number of seconds between renders.
This corresponds to the refresh rate of the scrolling animation.
For changes of this option to take effect, `good-scroll-mode' must be restarted."
  :group 'good-scroll
  :type 'float)

(defcustom good-scroll-duration 0.15
  "Duration of a scroll in seconds."
  :group 'good-scroll
  :type 'float)

(defcustom good-scroll-step 80
  "Number of pixel lines to scroll during a scroll step."
  :group 'good-scroll
  :type 'integer)

(defcustom good-scroll-point-jump 3
  "Number of text lines to move point when scrolling it off the window."
  :group 'good-scroll
  :type 'float)

(defcustom good-scroll-algorithm #'good-scroll-bezier
  "The scrolling animation algorithm to use.
If implementing your own algorithm, it should be a function with one argument,
a float from 0.0 to 1.0 representing the progress of the scroll.
The function should return a target position in pixel-lines relative to the top
of the window.
See the built-in algorithms for inspiration."
  :group 'good-scroll
  :type '(radio (function-item good-scroll-bezier)
                (function-item good-scroll-linear)
                function))

(defvar good-scroll--window nil
  "The window scrolled most recently.")

(defvar good-scroll--timer nil
  "Timer for render updates.")

(defvar good-scroll-destination nil
  "Destination of the current scroll.
The unit is pixel lines relative to the top of the window.
For example, -12 means scrolling down 12 pixels.")

(defvar good-scroll-traveled nil
  "Number of pixel lines traveled so far in the current scroll.")

(defvar good-scroll-start-time nil
  "Start time of the most recent scroll.")

(defvar good-scroll--direction 0
  "Direction of the most recent scroll.
This should be an integer. Positive means up and negative means down.")

;;;###autoload
(define-minor-mode good-scroll-mode
  "Good pixel line scrolling"
  :init-value nil
  :group 'scrolling
  :global t

  (if good-scroll-mode
      (setq mwheel-scroll-up-function #'good-scroll-up
            mwheel-scroll-down-function #'good-scroll-down
            good-scroll--timer
            (run-at-time 0 good-scroll-render-rate #'good-scroll--render))
    (progn
      (setq mwheel-scroll-up-function #'scroll-up
            mwheel-scroll-down-function #'scroll-down)
      (when (timerp good-scroll--timer)
        (cancel-timer good-scroll--timer)))))

(defun good-scroll-up (&optional _delta)
  "Scroll up one step.
The value of DELTA is ignored and exists only for compatibility with
`mwheel-scroll-up-function'."
  (interactive)
  (good-scroll--update 1))

(defun good-scroll-down (&optional _delta)
  "Scroll down one step.
The value of DELTA is ignored and exists only for compatibility with
`mwheel-scroll-down-function'."
  (interactive)
  (good-scroll--update -1))

(defun good-scroll--update (direction)
  "Begin a scroll in DIRECTION.
A negative DIRECTION means to scroll down. This is a helper function for
`good-scroll-up' and `good-scroll-down'."
  (unless (input-pending-p)
    (setq good-scroll-destination
          (+ (* direction good-scroll-step)
             ;; Reset destination if scroll changed direction
             (if (> (* direction good-scroll--direction) 0)
                 good-scroll-destination
               0))
          good-scroll-start-time (float-time)
          good-scroll-traveled 0
          good-scroll--direction direction
          good-scroll--window (selected-window))))

(defun good-scroll--render ()
  "Render an in-progress scroll.
Update the window's vscroll and position in the buffer based on the scroll
progress. This is called by the timer `good-scroll--timer' every
`good-scroll-render-rate' seconds."
  (when (eq (selected-window) good-scroll--window)
    (let* (
           (elapsed-time (- (float-time) good-scroll-start-time))
           (fraction-done (/ elapsed-time good-scroll-duration)))
      (unless (>= fraction-done 1.0)
        (let ((position-next (funcall good-scroll-algorithm fraction-done)))
          (cl-assert (<= (abs position-next)
                         (abs good-scroll-destination)))
          (good-scroll--go-to position-next)
          (setq good-scroll-traveled (+ good-scroll-traveled position-next)
                good-scroll-destination (- good-scroll-destination
                                            position-next)))))))

(defun good-scroll--go-to (pos)
  "Jump the window by POS pixel lines.
Update the window's vscroll and position in the buffer to instantly scroll to
POS, where POS is the index from the top of the window in pixel lines. POS can
be negative."
  (while (/= pos 0)
    (let* (
           ;; Number of pixels scrolled past the top of the first line.
           (vscroll (window-vscroll nil t))
           ;; Pixel height of the first line
           (line-height (save-excursion
                          (goto-char (window-start))
                          (line-pixel-height)))
           ;; Remaining number of pixels in the first line
           (rem (- line-height vscroll)))
      (setq pos
            (if (> pos 0)
                (good-scroll--go-to-up pos vscroll line-height rem)
              (good-scroll--go-to-down pos vscroll))))))

(defun good-scroll--go-to-up (pos vscroll line-height rem)
  "Partially jump the window up by POS pixel lines.
Return the remaining number of pixel lines to scroll.

The parameter VSCROLL is the selected window's vscroll,
LINE-HEIGHT is the height in pixels of the first line in the selected window,
and REM is the number of pixel lines from the vscroll to the end of the first
line in the selected window."
  (if (< (+ vscroll pos) line-height)
      ;; Done scrolling except for a fraction of a line.
      ;; Scroll a fraction of a line and terminate.
      (good-scroll--go-to-up-partial pos vscroll)
    ;; Scroll a full line
    (good-scroll--go-to-up-full pos rem)))

(defun good-scroll--go-to-up-partial (pos vscroll)
  "Increase the current window's vscroll by POS pixels.
Return zero. Assume VSCROLL + POS is less than the pixel height of the current
line and the current window's vscroll is VSCROLL."
  ;; Don't scroll if the last line is at the top of the window
  (when (/= (line-number-at-pos (point-max))
            (line-number-at-pos (window-start)))
    (set-window-vscroll nil (+ vscroll pos) t))
  0)

(defun good-scroll--go-to-up-full (pos rem)
  "Scroll the screen up by a full line.
Return the next target scroll position. Assume POS > REM, where REM is the
remaining amount of pixels from the top of the screen to the end of the top
line."
  (set-window-vscroll nil 0 t)
  ;; Move point out of the way
  (when (<= (line-number-at-pos (point))
            (+ (line-number-at-pos (window-start))
               good-scroll-point-jump))
    (vertical-motion good-scroll-point-jump))
  ;; Are we at the end of the buffer?
  (if (= (line-number-at-pos (point-max))
         (line-number-at-pos (window-start)))
      ;; We are!
      ;; Print a message and terminate.
      (progn
        (message (get #'end-of-buffer 'error-message))
        0)
    ;; We aren't.
    ;; Actually scroll one line
    (set-window-start nil (save-excursion
                            (goto-char (window-start))
                            (vertical-motion 1)
                            (point))
                      t)
    (- pos rem)))

(defun good-scroll--go-to-down (pos vscroll)
  "Partially jump the window down by POS pixel lines.
Return the remaining number of pixel lines to scroll. The parameter VSCROLL is
the selected window's vscroll."
  (if (<= (- pos) vscroll)
      ;; Done scrolling except for a fraction of a line.
      ;; Scroll a fraction of a line and terminate.
      (good-scroll--go-to-down-partial pos vscroll)
    ;; Scroll a full line
    (good-scroll--go-to-down-full pos vscroll)))

(defun good-scroll--go-to-down-partial (pos vscroll)
  "Change the current window's vscroll by POS pixels.
Return zero. Assume -POS <= VSCROLL."
  (set-window-vscroll nil (+ vscroll pos) t)
  0)

(defun good-scroll--go-to-down-full (pos vscroll)
  "Scroll the screen down by a full line.
Return the next target scroll position. Assume POS > VSCROLL."
  (set-window-vscroll nil 0 t)
  ;; Move point out of the way
  (when (<= (- (line-number-at-pos (window-end))
               good-scroll-point-jump)
            (line-number-at-pos (point)))
    (vertical-motion (- good-scroll-point-jump)))
  ;; Are we at the beginning of the buffer?
  (if (= (point-min) (window-start))
      ;; We are!
      ;; Print a message and terminate.
      (progn
        (message (get #'beginning-of-buffer 'error-message))
        0)
    ;; We aren't.
    ;; Actually scroll one line
    (set-window-start nil (save-excursion
                            (goto-char (window-start))
                            (vertical-motion -1)
                            (point))
                      t)
    (+ pos vscroll
       (save-excursion
         (goto-char (window-start))
         (line-pixel-height)))))

(provide 'good-scroll)

;;; good-scroll.el ends here
