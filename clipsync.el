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
;; clipnotify[0] and then using either xsel[1] or xclip[2]
;; to put new clipboard content in the kill ring.
;; It's inspired by the simple solution of clipmenu[3].
;;
;; From the autocutsel[4] doc:
;;
;; Recent desktop applications (GNOME, KDE, ...) use two selections:
;; the PRIMARY and the CLIPBOARD.  The PRIMARY selection is used when
;; you select some text with the mouse.  You usually paste it using the
;; middle button.  The CLIPBOARD selection is used when you copy text
;; by using, for example, the Edit/Copy menu.  You may paste it using
;; the Edit/Paste menu.

;; [0] https://github.com/cdown/clipnotify
;; [1] http://www.kfish.org/software/xsel/
;; [2] https://github.com/astrand/xclip
;; [3] https://github.com/cdown/clipmenu
;; [4] https://github.com/sigmike/autocutsel


;;; Code:
(require 'async)

;; PRIMARY is mouse selection
;; CLIPBOARD is copy text by Ctrl-C or Edit/Copy menu etc

(defvar clipsync--last-clipboard nil)
(defvar clipsync--last-primary nil)

;; (shell-command "xsel -v -o --primary" "outtest" "errtest")
;; (make-process "xsel-test" "*xsel-test" "xsel -o --primary")
;; (shell-command-to-string "xsel -o --primary")
;; FIXME: (shell-command-to-string "xsel -o --primary")
;;        sometimes just "hangs"?
(defun clipsync-get-selection (selection)
  "Get string from SELECTION.
SELECTION should be 'primary 'clipboard or 'secondary."
  (with-temp-buffer
    (make-process
     :name "xsel-test"
     :buffer (current-buffer)
     :command `("xsel" "-o" ,(concat "--" (symbol-name selection)))
     :noquery t
     ;;:stderr (get-buffer "*scratch*")
     :connection-type 'pipe)
    (sit-for 0.1 t)
    (goto-char (point-max))
    (forward-line -1)
    (unless (eq (point) (point-min))
      (forward-char -1))
    (buffer-substring (point-min) (point))))


;; FIXME: Strange behavior when using primary AND clipboard
;; as Emacs fills clipboard itself
;; FIXME: Only do when left mouse button is not pressed?!
(defun clipsync-xsel-kill-new ()
  "Check selections for new strings and put them in the kill ring."
  (let ((primary (clipsync-get-selection 'primary)))
    ;; Check if the primary selection was changed
    (message "--- New selection")
    (if (not (string-equal primary clipsync--last-primary))
        (progn
          (message "primary")
          (setq clipsync--last-primary primary)
          (kill-new primary))
      ;; Check if the clipboard selection was changed
      (let ((clipboard (clipsync-get-selection 'clipboard)))
        (unless (string-equal clipboard clipsync--last-clipboard)
          (message "clipboard")
          (setq clipsync--last-clipboard clipboard)
          (kill-new clipboard))))))


;;;###autoload
(define-minor-mode clipsync-mode
  "Synchronize your primary and clipboard selection with the kill ring."
  :lighter" clipsync"
  :global t
  (when clipsync-mode
    (clipsync-start-clipnotify)))


(defun clipsync--clipnotify-finish-func (_proc)
  "This function is called when clipnotify finishes.
Which means we check for new selections and start again."
  (with-current-buffer (window-buffer (selected-window))
    (sit-for 1 t)
    ;; We only want to copy stuff when we're in an X11 window
    ;; Emacs can handle the clipboard / kill-ring itself
    (when (eq major-mode 'exwm-mode)
      (clipsync-xsel-kill-new)))
  ;; Start clipnotify again
  (when clipsync-mode
    (clipsync-start-clipnotify)))

(defun clipsync-start-clipnotify ()
  "Start clipsync."
  (async-start-process "clipnotify" "clipnotify" #'clipsync--clipnotify-finish-func))


(provide 'clipsync)
;;; clipsync.el ends here
