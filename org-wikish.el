;;; org-wikish.el --- simple org wiki
;;
;; Filename: org-wikish.el
;; Description: Supplies simple commands and keybindings for building a personal wiki in org-mode
;; Author: Benaiah Mischenko
;; Maintainer: Benaiah Mischenko
;; Created: Feb 10 2017
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (org "8.2.10"))
;; Last-Updated: Tue May 12 2016
;;           By: Benaiah Mischenko
;;     Update #: 1
;; URL: http://github.com/benaiah/org-wikish
;; Doc URL: http://github.com/benaiah/org-wikish
;; Keywords: outlines
;; Compatibility: GNU Emacs: 24.x, 25.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This supplies simple commands and keybindings for building a
;; personal wiki in org-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)

;;;###autoload
(defvar org-wikish-wiki-directory "/your/directory/here/")

(defun org-wikish-split-camelcase (word)
  (let ((case-fold-search nil))
    (split-string
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1___\\2" word) "___")))

(defun org-wikish-word-to-filename (word)
  (string-join (list (string-join
                      (mapcar #'downcase (org-wikish-split-camelcase word)) "-") ".org")))

(defun org-wikish-file-name-to-word (filename)
  (string-join
   (mapcar #'capitalize
           (split-string (replace-regexp-in-string "\\.org$" "" filename) "-"))))

(defun org-wikish-file-name-to-path (filename)
  (concat (file-name-as-directory org-wikish-wiki-directory) filename))

(defun org-wikish-file-name (path)
  (concat (file-name-base path) (file-name-extension path)))

(defun org-wikish-maybe-relative-path (path &optional buffer-path)
  (let ((buffer-path (if buffer-path buffer-path buffer-file-name)))
    (if (and (file-name-absolute-p path)
             buffer-path
             (org-wikish-in-wiki-p buffer-path))
        (let ((newpath (file-relative-name path org-wikish-wiki-directory)))
          (concat "./" newpath))
      path)))

(defun org-wikish-word-to-path (word)
  (org-wikish-maybe-relative-path
   (concat (file-name-as-directory org-wikish-wiki-directory)
           (org-wikish-word-to-filename word))))

(defun org-wikish-word-to-link (word)
  (string-join (list "[[" (org-wikish-word-to-path word) "][" word "]]")))

(defun org-wikish-in-wiki-p (path)
  (string-equal
   (file-name-directory path)
   (file-name-as-directory org-wikish-wiki-directory)))

(defun org-wikish-link-word-at-point ()
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
               (filename (org-wikish-word-to-filename word))
               (link (org-wikish-word-to-link word)))
          (delete-region (car bounds) (cdr bounds))
          (insert link))))))

(defun org-wikish-ensure-page-exists (word)
  (let ((path (org-wikish-word-to-path word)))
    (if (not (file-exists-p path))
        (write-region "" nil path))))

(defun org-wikish-open-link-at-point ()
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((word (thing-at-point 'symbol)))
        (org-wikish-link-word-at-point)
        (org-wikish-ensure-page-exists word)
        (org-open-at-point))
    (org-wikish-link-word-at-point-and-enter)))

(defun org-wikish-page-completion-list ()
  (mapcar (lambda (filename)
            (org-wikish-file-name-to-word filename))
          (cl-remove-if-not (lambda (filename) (string-match "\\.org$" filename))
                            (directory-files org-wikish-wiki-directory))))

;;;###autoload
(defun org-wikish-find-page (word)
  (interactive (list (completing-read "Org file: " (org-wikish-page-completion-list))))
  (org-open-file (org-wikish-word-to-path word)))

(defun org-wikish-export-all-to-html ()
  (interactive)
  (save-excursion
    (mapc
     (lambda (file)
       (with-temp-buffer
         (find-file file)
         (org-html-export-to-html)))
     (file-expand-wildcards (concat (file-name-as-directory org-wikish-wiki-directory)
                                    "*.org")))))

;;;###autoload
(define-minor-mode org-wikish-mode
  "Toggle org-wikish mode."
  :init-value nil
  :lighter " Wikish"
  :keymap'(([C-c w g] . org-wikish-link-word-at-point)
           ([C-c w o] . org-wikish-open-page-at-point)))

;;;###autoload
(defun org-wikish-recommended-global-keybindings ()
  (define-key global-map (kbd "C-c w f") #'org-wikish-find-page))

(provide 'org-wikish)

;;; org-wikish.el ends here
