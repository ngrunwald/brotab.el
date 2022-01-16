;;; brotab.el --- Emacs Frontend for brotab browser extension. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/brotab.el
;; Created: 2022
;; Version: 0.1.0
;; Keywords: browser
;; Package-Requires: ((tablist "20200427.2205") (s "20210616.619") (dash "20210826.1149"))

;; This file is NOT part of GNU Emacs.

;; brotab.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; brotab.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with brotab.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs Frontend for the brotab browser extension.

;;; Code:
(require 'seq)
(require 's)
(require 'tabulated-list)
(require 'tablist)
(require 'subr-x)
(require 'cl-lib)
(require 'dash)

(defgroup brotab nil
  "brotab customization group."
  :prefix "brotab-"
  :group 'external)

(defcustom brotab-program "bt" "Name of the brotab program"
  :type 'string)

(defcustom brotab-list-buffer-name "*Brotab*" "Name of the brotab list buffer"
  :type 'string)


(defun brotab--parse-tab-line (line)
  (-let* (((id title url) (s-split "\t" line t))
          (parsed (url-generic-parse-url url))
          (host (url-host parsed))
          (client-id (s-left 2 id)))
    `((:tab-id . ,id)
      (:client-id . ,client-id)
      (:url . ,url)
      (:host . ,host)
      (:title . ,title))))

(defun brotab--collect-tabs ()
  (let ((raw (shell-command-to-string (s-concat brotab-program " clients"))))
    (thread-last (process-lines brotab-program "list")
      (seq-map 'brotab--parse-tab-line)
      (seq-map (lambda (tab) (let* ((client-id (alist-get :client-id tab))
                                    (browser (cdr (assoc client-id (brotab--clients-map raw)))))
                               (seq-concatenate 'list tab browser)))))))

(defun brotab--parse-client-line (line)
  (-let (((id endpoint pid generic) (s-split "\t" line t)))
    `((:id . ,id)
      (:endpoint . ,endpoint)
      (:pid . ,pid)
      (:generic-name . ,generic))))

(defun brotab--client-to-browser (client)
  (let* ((ppid (brotab--get-parent-from-pid (alist-get :pid client)))
         (pname (brotab--get-name-from-pid ppid)))
    (seq-concatenate 'list client
                      `((:browser-pid . ,ppid)
                        (:browser-name . ,pname)))))

(defvar brotab--clients-map-cache nil)

(defun brotab--clients-map (raw)
  (if (string= raw (car brotab--clients-map-cache))
      (cdr brotab--clients-map-cache)
    (let* ((lines (s-lines (s-trim raw)))
           (clients (thread-last lines
                      (seq-map 'brotab--parse-client-line)
                      (seq-map 'brotab--client-to-browser)))
           (all-clients (seq-map (lambda (b) `(,(cdr (assoc :id b)) . ,b)) clients)))
      (setq brotab--clients-map-cache `(,raw . ,all-clients))
      all-clients)))

(defun brotab--select-tab (tab-id)
  (call-process  brotab-program
                 nil nil nil
                 "activate"
                 "--focused"
                 tab-id))

(defun brotab--kill-tab (tab-id)
  (call-process  brotab-program
                 nil nil nil
                 "close"
                 tab-id))

(defun brotab--get-parent-from-pid (pid)
  "Get the parent PID of given PID."
  (let ((ppid (shell-command-to-string (format "ps -o ppid= -p %s" pid))))
    (when (string-match "[0-9]" ppid)
      (string-trim ppid))))

(defun brotab--get-name-from-pid (pid)
  "Get process name from PID."
  (let ((nam (string-trim (shell-command-to-string (format "ps -o comm= -p %s" pid)))))
    (when (not (string-blank-p nam))
      nam)))

(defun brotab--browser-name (tab)
  (s-concat (alist-get :browser-name tab) "-" (s-replace-regexp "\\.[0-9]+$" "" (alist-get :tab-id tab))))

(defun brotab--format-tab-line (tab)
  (let* ((id (propertize (alist-get :tab-id tab)
                         'tab tab))
         (bname (brotab--browser-name tab))
         (columns (vector
                   (alist-get :title tab)
                   (alist-get :host tab)
                   bname
                   (alist-get :url tab))))
    (list id columns)))

(defun brotab--tabs-list ()
  (let ((tabs (brotab--collect-tabs)))
    (seq-map 'brotab--format-tab-line tabs)))

(defun brotab--tabs-list-refresh ()
  (interactive)
  (setq-local tabulated-list-entries (brotab--tabs-list)))

(defun brotab-list-select-tab ()
  (interactive)
  (let ((tab-id (tabulated-list-get-id)))
    (brotab--select-tab tab-id)))

(defun brotab-list-kill-tab ()
  (interactive)
  (let ((ids (seq-map 'car (tablist-get-marked-items))))
    (seq-each 'brotab--kill-tab ids))
  (tablist-revert))

(defvar brotab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k") 'brotab-list-kill-tab)
    map)
  "Keymap for `brotab-mode'")

(define-derived-mode brotab-mode tabulated-list-mode "brotab-mode"
  "Major mode for handling brotab."
  (setq tabulated-list-format [("Title" 60 t)
                               ("Host" 24 t)
                               ("Browser" 12 t)
                               ("Url" 0 t)])
  (setq-local tabulated-list-entries (brotab--tabs-list))
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'brotab--tabs-list-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode)
  (local-set-key [return] 'brotab-list-select-tab)
    (local-set-key [remap tablist-do-kill-lines] 'brotab-list-kill-tab)
  (tablist-revert)
  (view-mode))

;;;###autoload
(defun brotab-list-tabs ()
  "Displays the list of open browser tabs for batch actions."
  (interactive)
  (switch-to-buffer brotab-list-buffer-name)
  (brotab-mode))

(provide 'brotab)
;;; brotab.el ends here
