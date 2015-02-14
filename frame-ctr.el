;;; frame-ctr.el --- A tool to control frame size, position, and font size
;;
;; Copyright (C) 2015 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Repository: nil
;; Keywords: frame-cmds, frame, size, position
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'frame-cmds)
(eval-when-compile (require 'cl))

(defcustom move-frame-pixel-menubar-offset 22
  "Offset of the menubar. The default height is 22 for MacOSX"
  :type 'integer
  :group 'taka-frame-control)

(defcustom move-frame-pixel-offset '(0 . 0)
  "Offset of the center position"
  :type 'sexp
  :group 'takaxp-frame-control)

(defcustom auto-move-frame-to-center nil
  "Toggle status of moving frame to center"
  :type 'boolean
  :group 'takaxp-frame-control)

(defcustom min-frame-height 16
  "The minimum height"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom fullscreen-fontsize 28
  "Font size will be used for fullscreen"
  :type 'integer
  :group 'takaxp-frame-control)

;;;###autoload
(defun toggle-auto-move-frame-to-center ()
  "Change whether move the frame to center automatically"
  (interactive)
  (cond (auto-move-frame-to-center
	 (setq auto-move-frame-to-center nil)
	 (message "Toggle auto move OFF"))
	(t (setq auto-move-frame-to-center t)
	   (message "Toggle auto move ON"))))

;;;###autoload
(defun move-frame-to-horizontal-center ()
  "Move the current frame to the horizontal center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
		      (+ (car move-frame-pixel-offset)
			 (/ (- (display-pixel-width) (frame-pixel-width)) 2))
		      (frame-parameter (selected-frame) 'top)))

;;;###autoload
(defun move-frame-to-vertical-center ()
  "Move the current frame to the vertical center of the window display."
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)
		      (+ (cdr move-frame-pixel-offset)
			 (/ (- (display-pixel-height)
			       (frame-pixel-height)) 2))))

;;;###autoload
(defun move-frame-to-edge-top ()
  "Move the current frame to the top of the window display"
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)
		      0))

;;;###autoload
(defun move-frame-to-edge-bottom ()
  "Move the current frame to the top of the window display
   If you find the frame is NOT moved to the bottom exactly,
   Please set `move-frame-pixel-menubar-offset'.
   22 is the default value for MacOSX"
  (interactive)
  (set-frame-position (selected-frame)
		      (frame-parameter (selected-frame) 'left)		      
		      (- (- (display-pixel-height) (frame-pixel-height))
			 move-frame-pixel-menubar-offset)))

;;;###autoload
(defun move-frame-to-center ()
  "Move the current frame to the center of the window display."
  (interactive)
  (let
      ((prev-pos-x (frame-parameter (selected-frame) 'left))
       (prev-pos-y (frame-parameter (selected-frame) 'top))
       (center-pos-x
	(+ (car move-frame-pixel-offset)
	   (/ (- (display-pixel-width) (frame-pixel-width)) 2)))
       (center-pos-y
	(+ (cdr move-frame-pixel-offset)
	   (/ (- (display-pixel-height) (frame-pixel-height)) 2))))
    (set-frame-position (selected-frame) center-pos-x center-pos-y)
    (message "Frame move: from (%s, %s) to (%s, %s)"
	     prev-pos-x
	     prev-pos-y
	     (frame-parameter (selected-frame) 'left)
	     (frame-parameter (selected-frame) 'top))))

;;;###autoload
(defun move-frame-with-user-specify (&optional arg)
  "Move the frame to somewhere (default: 0,0).
   Use prefix to specify the destination position."
  (interactive "P")
  (let ((pos-x 0)
	(pos-y move-frame-pixel-menubar-offset))
    (when arg
      (setq pos-x (string-to-number
		   (read-from-minibuffer
		    (format "X: from %s to "
			    (frame-parameter (selected-frame) 'left)))))
      (setq pos-y (string-to-number
		   (read-from-minibuffer
		    (format "Y: from %s to "
			    (frame-parameter (selected-frame) 'top))))))
    (set-frame-position (selected-frame) pos-x pos-y)
    (message "Frame move: (%s, %s)"
	     (frame-parameter (selected-frame) 'left)
	     (frame-parameter (selected-frame) 'top))))

;;;###autoload
(defun max-frame-height ()
  "Return the maximum height based on screen size."
  (interactive)
  (/ (- (x-display-pixel-height) 64) (frame-char-height)))

;;;###autoload
(defun min-frame-height ()
  "Return the minimum height of frame"
  (interactive)
  min-frame-height)

;;;###autoload
(defun reset-frame-height (new-height)
  "Reset the hight of the current frame."
  (interactive
   (list (string-to-number
	  (read-string "New Height: " (number-to-string (frame-height))))))
  (let ((min-height (min-frame-height))
	(max-height (max-frame-height)))
    (when (> new-height max-height)
      (setq new-height max-height)
      (message "Force set the height %s." new-height))
    (when (< new-height min-height)
      (setq new-height min-height)
      (message "Force set the height %s." new-height))
    (let ((height (floor new-height)))
      (set-frame-height (selected-frame) height)
      (message "Height: %s" height))))

;;;###autoload
(defun fit-frame-to-fullscreen ()
  "Change font size and expand height to fit full"
  (interactive)
  (set-font-size fullscreen-fontsize)
  (reset-frame-height (max-frame-height)))

(defvar frame-ctr-height-ring nil)
;;;###autoload
(defun frame-ctr-make-height-ring (heights)
  "Cycle change the height of the current frame."
  (setq frame-ctr-height-ring (copy-sequence heights)))
  
;;;###autoload
(defun frame-ctr-open-height-ring ()
  (interactive)
  (reset-frame-height (car frame-ctr-height-ring))
  (setq frame-ctr-height-ring
	(append (cdr frame-ctr-height-ring)
		(list (car frame-ctr-height-ring)))))

(provide 'frame-ctr)
