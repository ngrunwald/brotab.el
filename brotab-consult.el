;;; brotab-consult.el --- Consult integration for brotab browser extension. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/brotab.el
;; Created: 2022
;; Version: 0.1.0
;; Keywords: browser consult completion
;; Package-Requires: ((s "20210616.619") (consult "20220111.1857"))

;; This file is NOT part of GNU Emacs.

;; brotab-consult.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; brotab-consult.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with brotab-consult.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs Frontend for the brotab browser extension.

;;; Code:
(require 'seq)
(require 'subr-x)
(require 'consult)
(require 'brotab)

(defun consult-brotab--lookup (_input cands cand)
  (seq-find (lambda (x) (string= cand x)) cands))


(defun brotab-embark--select-tab (cand)
  (let ((tab-id (get-text-property 0 'tab-id cand)))
    (brotab--select-tab tab-id)))


(defun brotab-embark--kill-tab (cand)
  (let ((tab-id (get-text-property 0 'tab-id cand)))
    (brotab--kill-tab tab-id)))


(defun consult-brotab--format-tab (max-size tab)
  (let* ((title (alist-get :title tab))
         (host (alist-get :host tab))
         (tab-id (alist-get :tab-id tab))
         (browser-name (brotab--browser-name tab)))
    (thread-first (format (format "%%-%ds [%%s]" (+ max-size 5)) title host)
      (propertize 'tab-id tab-id
                   'host host
                   'browser-name browser-name))))

;;;###autoload
(defun consult-brotab ()
  "Completing interface to browser tabs."
  (interactive)
  (let* ((tabs (brotab--collect-tabs))
         (max-size (apply 'max (seq-map (lambda (tab) (length (cdr (assoc :title tab)))) tabs)))
         (candidates (seq-map (-partial 'consult-brotab--format-tab max-size) tabs))
         (selected (consult--read candidates
                                  :prompt "Tab: "
                                  :category 'browser-tab-brotab
                                  :require-match t
                                  :group (lambda (cand transform)
                                           (if transform
                                               cand
                                             (get-text-property 0 'browser-name cand)))
                                  :annotate (lambda (cand)
                                              (format " <%s>"(get-text-property 0 'browser-name cand)))
                                  :lookup 'consult-brotab--lookup)))
    (when selected
      (brotab--select-tab (get-text-property 0 'tab-id selected)))))

(provide 'brotab-consult)
;;; brotab-consult.el ends here
