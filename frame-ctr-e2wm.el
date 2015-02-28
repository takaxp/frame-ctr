;;; frame-ctr-e2wm.el --- A tool to control frame size, position, and division
;;
;; Copyright (C) 2015 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Repository: nil
;; Keywords: frame-ctr, e2wm, frame-cmds, frame, size, position
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

(require 'e2wm)
(require 'frame-ctr)
(require 'frame-cmds)

(defcustom frame-width-single 80
  "The width of the current frame as the default value"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-width-double 163
  "The width of the current frame (double size)"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-height-small 35
  "The height of the current frame as the default value"
  :type 'integer
  :group 'takaxp-frame-control)

(defcustom frame-height-tall 60
  "The height of the current frame (tall version)"
  :type 'integer
  :group 'takaxp-frame-control)

;;;###autoload
(defun change-frame-width-double ()
  "Change the frame width to double"
  (interactive)
  (set-frame-width (selected-frame) frame-width-double))

;;;###autoload
(defun change-frame-width-single ()
  "Change the frame width to double"
  (interactive)
  (set-frame-width (selected-frame) frame-width-single))

;;;###autoload
(defun change-frame-single-window (&optional arg)
  "Change the width of the frame to a single width frame"
  (interactive "P")
  (let 
      ((selected-buffer (current-buffer)))
    (e2wm:stop-management)
    (cond (arg
	   (set-frame-size (selected-frame)
			   frame-width-single frame-height-tall))
	  (t
	   (set-frame-size (selected-frame)
			   frame-width-single frame-height-small)))
    (switch-to-buffer selected-buffer)
    (when auto-move-frame-to-center
      (move-frame-to-center))))

;;;###autoload
(defun change-frame-double-window (&optional arg)
  "Change the width of the frame to double width frame"
  (interactive "P")
  (cond (arg
	 (set-frame-size (selected-frame)
			 frame-width-double frame-height-tall))
	(t
	 (set-frame-size (selected-frame)
			 frame-width-double frame-height-small)))
  (when auto-move-frame-to-center
    (move-frame-to-center))
  (e2wm:start-management)
  (e2wm:dp-two))

(provide 'frame-ctr-e2wm)
