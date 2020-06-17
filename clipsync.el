;;; clipsync.el --- Synchronize your clipboard selection with the kill ring -*- lexical-binding: t -*-

;; Copyright (c) 2020 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/clipsync.el
;; Keywords: clipboard, selection, xsel, xclip, clipnotify, convenience, tools
;; Version: 0.1
;; Package-Requires: ((emacs "25.2"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `clipsync.el' synchronizes your primary and clipboard selection with
;; the Emacs kill ring by waiting for new clipboard events with
;; clipnotify[0] and then adding new clipboard content in the kill ring.
;; It's inspired by the simple solution of clipmenu[1].
;;
;; Some info what's CLIPBOARD and PRIMARY, see the autocutsel[2] doc:
;;
;; Recent desktop applications (GNOME, KDE, ...) use two selections:
;; the PRIMARY and the CLIPBOARD.  The PRIMARY selection is used when
;; you select some text with the mouse.  You usually paste it using the
;; middle button.  The CLIPBOARD selection is used when you copy text
;; by using, for example, the Edit/Copy menu.  You may paste it using
;; the Edit/Paste menu.

;; [0] https://github.com/cdown/clipnotify
;; [1] https://github.com/cdown/clipmenu
;; [2] https://github.com/sigmike/autocutsel


;;; Code:
(require 'async)
(require 'select)


;; PRIMARY is mouse selection
;; CLIPBOARD is copy text by Ctrl-C or Edit/Copy menu etc

(defun clipsync-get-selection ()
  "Get string from clipboard or primary selection."
  (let ((select-enable-primary t)
        (select-enable-clipboard t))
    (gui-selection-value)))


;;;###autoload
(define-minor-mode clipsync-mode
  "Synchronize your primary and clipboard selection with the kill ring."
  :lighter" clipsync"
  :global t
  (when clipsync-mode
    (setq select-active-regions nil)
    (clipsync-start-clipnotify)))


(defun clipsync--clipnotify-finish-func (_proc)
  "This function is called when clipnotify finishes.
Which means we check for new selections and start again."
  (with-current-buffer (window-buffer (selected-window))
    (sit-for 0.5 t)
    ;; We only want to copy stuff when we're in an X11 window
    ;; Emacs can handle the clipboard / kill-ring itself
    (when (eq major-mode 'exwm-mode)
      (when-let ((selection (clipsync-get-selection)))
        (kill-new selection))))
  ;; Start clipnotify again
  (when clipsync-mode
    (clipsync-start-clipnotify)))

(defun clipsync-start-clipnotify ()
  "Start clipsync."
  (async-start-process "clipnotify" "clipnotify" #'clipsync--clipnotify-finish-func))


(provide 'clipsync)
;;; clipsync.el ends here
