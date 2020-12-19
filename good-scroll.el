;;; good-scroll.el --- Good scrolling                -*- lexical-binding: t; -*-

(defvar good-scroll-render-rate (/ 1.0 30.0))
(defvar good-scroll-duration 0.15)
(defvar good-scroll-step 40)
(defvar good-scroll-point-jump 3)

(defvar good-scroll--window nil)
(defvar good-scroll--timer)
(defvar good-scroll--destination)
(defvar good-scroll--traveled)
(defvar good-scroll--start-time)
(defvar good-scroll--direction 0)

;;;###autoload
(define-minor-mode good-scroll-mode
  "Good scrolling"
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

(defun good-scroll-up (&optional delta)
  (interactive)
  (good-scroll--update (or delta 1)))

(defun good-scroll-down (&optional delta)
  (interactive)
  (good-scroll--update (- (or delta 1))))

(defun good-scroll--update (delta)
  (unless (input-pending-p)
    (setq good-scroll--destination
          (+ (* delta good-scroll-step)
             (if (> (* delta good-scroll--direction) 0)
                 good-scroll--destination
               0))
          good-scroll--start-time (float-time)
          good-scroll--traveled 0
          good-scroll--direction delta
          good-scroll--window (selected-window))))

(defun good-scroll--render ()
  (when (eq (selected-window) good-scroll--window)
    (let* (
           (elapsed-time (- (float-time) good-scroll--start-time))
           (fraction-done (/ elapsed-time good-scroll-duration))
           (position-next (round (- (* fraction-done
                                       (+ good-scroll--traveled
                                          good-scroll--destination))
                                    good-scroll--traveled))))
      (unless (>= fraction-done 1.0)
        (setq good-scroll--traveled (+ good-scroll--traveled position-next)
              good-scroll--destination (- good-scroll--destination position-next))
        (good-scroll--go-to position-next)))))

(defun good-scroll--go-to (pos)
  (while (/= pos 0)
    (let* (
           (vscroll (window-vscroll nil t))
           (line-height (save-excursion
                          (goto-char (window-start))
                          (line-pixel-height)))
           (rem (- line-height vscroll)))
      (setq pos
            (if (> pos 0)
                (good-scroll--go-to-up pos vscroll line-height rem)
              (good-scroll--go-to-down pos vscroll))))))

(defun good-scroll--go-to-up (pos vscroll line-height rem)
  (if (< (+ vscroll pos) line-height)
      ;; Done scrolling except for a fraction of a line.
      ;; Scroll a fraction of a line and terminate.
      (good-scroll--go-to-up-partial pos vscroll)
    ;; Scroll a full line
    (good-scroll--go-to-up-full pos rem)))

(defun good-scroll--go-to-up-partial (pos vscroll)
  (when (/= (point-max) (window-start))
    (set-window-vscroll nil (+ vscroll pos) t))
  0)

(defun good-scroll--go-to-up-full (pos rem)
  (set-window-vscroll nil 0 t)
  ;; Move point out of the way
  (when (<= (line-number-at-pos (point))
            (+ (line-number-at-pos (window-start))
               good-scroll-point-jump))
    (forward-line good-scroll-point-jump))
  ;; Are we at the end of the buffer?
  (if (= (point-max) (window-start))
      ;; We are!
      ;; Print a message and terminate.
      (progn
        (message (get #'end-of-buffer 'error-message))
        0)
    ;; We aren't.
    ;; Actually scroll one line
    (set-window-start nil (save-excursion
                            (goto-char (window-start))
                            (forward-line)
                            (point))
                      t)
    (- pos rem)))

(defun good-scroll--go-to-down (pos vscroll)
  (if (<= (- pos) vscroll)
      ;; Done scrolling except for a fraction of a line.
      ;; Scroll a fraction of a line and terminate.
      (good-scroll--go-to-down-partial pos vscroll)
    ;; Scroll a full line
    (good-scroll--go-to-down-full pos vscroll)))

(defun good-scroll--go-to-down-partial (pos vscroll)
  (set-window-vscroll nil (+ vscroll pos) t)
  0)

(defun good-scroll--go-to-down-full (pos vscroll)
  (set-window-vscroll nil 0 t)
  ;; Move point out of the way
  (when (<= (- (line-number-at-pos (window-end))
               good-scroll-point-jump)
            (line-number-at-pos (point)))
    (forward-line (- good-scroll-point-jump)))
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
                            (forward-line -1)
                            (point))
                      t)
    (+ pos vscroll
       (save-excursion
         (goto-char (window-start))
         (line-pixel-height)))))
