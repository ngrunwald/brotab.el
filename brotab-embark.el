;;; brotab-embark.el --- Embark integration for brotab browser extension. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nils Grunwald <github.com/ngrunwald>
;; Author: Nils Grunwald
;; URL: https://github.com/ngrunwald/brotab.el
;; Created: 2022
;; Version: 0.1.0
;; Keywords: browser consult completion
;; Package-Requires: ((embark "20220111.1739"))

;; This file is NOT part of GNU Emacs.

;; brotab-embark.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; brotab-embark.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with brotab-embark.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs Frontend for the brotab browser extension.

;;; Code:
(require 'embark)
(require 'brotab)

(defun brotab-embark--select-tab (cand)
  (let ((tab-id (get-text-property 0 'tab-id cand)))
    (brotab--select-tab tab-id)))

(defun brotab-embark--kill-tab (cand)
  (let ((tab-id (get-text-property 0 'tab-id cand)))
    (brotab--kill-tab tab-id)))

(embark-define-keymap  embark-browser-tab-brotab-actions
  "Keymap for actions for brotab browser tabs."
  ("b" brotab-embark--select-tab)
  ("k" brotab-embark--kill-tab))

(add-to-list 'embark-keymap-alist '(browser-tab-brotab . embark-browser-tab-brotab-actions))

(provide 'brotab-embark)
;;; brotab-embark.el ends here
