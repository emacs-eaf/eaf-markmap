;;; eaf-markmap.el --- Vue demo

;; Filename: eaf-markmap.el
;; Description: Vue demo
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2021, Andy Stewart, all rights reserved.
;; Created: 2021-08-01 10:30:42
;; Version: 0.1
;; Last-Updated: 2021-08-01 10:30:42
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/eaf-markmap.el
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Vue demo
;;

;;; Installation:
;;
;; Put eaf-markmap.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'eaf-markmap)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET eaf-markmap RET
;;

;;; Change log:
;;
;; 2021/08/01
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

;;;###autoload
(defun eaf-open-markmap ()
  "Open EAF vue demo"
  (interactive)
  (eaf-open "eaf-markmap" "markmap"))

(defcustom eaf-markmap-keybinding
  '(("<f12>" . "open_devtools"))
  "The keybinding of EAF Vue demo."
  :type 'cons)

(defcustom eaf-markmap-extension-list
  '("md")
  "The extension list of markmap application."
  :type 'cons)

(defun eaf--markmap-preview-display (buf)
  "Given BUF, split window to show file and previewer."
  (eaf-split-preview-windows
   (buffer-local-value
    'eaf--buffer-url buf))
  (switch-to-buffer buf)
  (other-window +1))

(defun eaf--markmap-hook ()
  (eaf--markmap-sync-content)
  (run-with-idle-timer 1 t 'eaf--markmap-sync-content))

(defun eaf--markmap-sync-content ()
  (let ((markdown-buffer (current-buffer))
        (markdown-buffer-file (buffer-file-name)))
    (eaf-for-each-eaf-buffer
     (when (and (string-equal eaf--buffer-app-name "markmap")
                (string-equal eaf--buffer-url markdown-buffer-file))
       (eaf-call-async "execute_function_with_args"
                       eaf--buffer-id
                       "sync_content"
                       (eaf--encode-string
                        (with-current-buffer markdown-buffer
                          (buffer-string)))
                       )))))

(add-to-list 'eaf-app-hook-alist '("markmap" . eaf--markmap-hook))

(add-to-list 'eaf-app-binding-alist '("markmap" . eaf-markmap-keybinding))

(setq eaf-markmap-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("markmap" . eaf-markmap-module-path))

(add-to-list 'eaf-preview-display-function-alist '("markmap" . eaf--markmap-preview-display))

(add-to-list 'eaf-app-extensions-alist '("markmap" . eaf-markmap-extension-list))

(provide 'eaf-markmap)

;;; eaf-markmap.el ends here
