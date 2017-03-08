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

(defcustom org-wikish-wiki-directory "/your/directory/here/"
  "Directory for org-wikish files.")

(defun org-wikish-split-camelcase (word)
  "Get a list of subwords from the CamelCaseWord WORD."
  (let ((case-fold-search nil))
    (split-string
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1___\\2" word) "___")))

(defun org-wikish-word-to-filename (word)
  "Get a dash-separated org filename from the CamelCaseWord WORD."
  (string-join (list (string-join
                      (mapcar #'downcase (org-wikish-split-camelcase word)) "-") ".org")))

(defun org-wikish-file-name-to-word (filename)
  "Get a CamelCaseWord from FILENAME."
  (string-join
   (mapcar #'capitalize
           (split-string (replace-regexp-in-string "\\.org$" "" filename) "-"))))

(defun org-wikish-file-name-to-path (filename)
  "Get a full absolute path to a wiki page from FILENAME, using `org-wikish-wiki-directory'."
  (concat (file-name-as-directory org-wikish-wiki-directory) filename))

(defun org-wikish-file-name (path)
  "Get the filename part of PATH."
  (concat (file-name-base path) (file-name-extension path)))

(defun org-wikish-maybe-relative-path (path &optional buffer-path)
  "Return a relative version of the absolute path PATH if the current buffer's path (or the optional BUFFER-PATH) is in the `org-wikish-wiki-directory', and PATH otherwise."
  (let ((buffer-path (if buffer-path buffer-path buffer-file-name)))
    (if (and (file-name-absolute-p path)
             buffer-path
             (org-wikish-in-wiki-p buffer-path))
        (let ((newpath (file-relative-name path org-wikish-wiki-directory)))
          (concat "./" newpath))
      path)))

(defun org-wikish-word-to-path (word)
  "Get an absolute path to a page from the CamelCaseWord WORD."
  (org-wikish-maybe-relative-path
   (concat (file-name-as-directory org-wikish-wiki-directory)
           (org-wikish-word-to-filename word))))

(defun org-wikish-word-to-link (word)
  "Get the text of an org link to a wiki page from the CamelCaseWord WORD."
  (string-join (list "[[" (org-wikish-word-to-path word) "][" word "]]")))

(defun org-wikish-in-wiki-p (path)
  "Return whether the given PATH is in the `org-wikish-wiki-directory'."
  (string-equal
   (file-name-directory path)
   (file-name-as-directory org-wikish-wiki-directory)))

(defun org-wikish-link-word-at-point ()
  "Turn the word at point into an org link to the corresponding page."
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
  "Create a wiki page for WORD if it doesn't exist."
  (let ((path (org-wikish-word-to-path word)))
    (if (not (file-exists-p path))
        (write-region "" nil path))))

(defun org-wikish-open-page-at-point ()
  "Turn the word at point into an org link to the corresponding page, and go to that page."
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (org-open-at-point)
    (let ((word (thing-at-point 'symbol)))
      (org-wikish-link-word-at-point)
      (org-wikish-ensure-page-exists word)
      (org-open-at-point))))

(defun org-wikish-page-completion-list ()
  "Get a list of wiki pages as CamelCaseWords."
  (mapcar (lambda (filename)
            (org-wikish-file-name-to-word filename))
          (cl-remove-if-not (lambda (filename) (string-match "\\.org$" filename))
                            (directory-files org-wikish-wiki-directory))))

;;;###autoload
(defun org-wikish-find-page (word)
  "Go to or create the wiki page for WORD with ‘completing-read’."
  (interactive (list (completing-read "Org file: " (org-wikish-page-completion-list))))
  (org-open-file (org-wikish-word-to-path word)))

(defun org-wikish-export-all-to-html ()
  "Export all wiki pages to HTML pages in the same directory."
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
  :keymap `((,(kbd "C-c w g") . org-wikish-link-word-at-point)
            (,(kbd "C-c w o") . org-wikish-open-page-at-point)))

;;;###autoload
(defun org-wikish-recommended-global-keybindings ()
  "Bind recommended global keys."
  (define-key global-map (kbd "C-c w f") #'org-wikish-find-page))

(provide 'org-wikish)

;;; org-wikish.el ends here
