(require 'cl)
(require 'subr-x)
(require 'org)

(defvar org-wikish-notes-directory "/your/directory/here/")

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
  (concat (file-name-as-directory org-wikish-notes-directory) filename))

(defun org-wikish-word-to-path (word)
  (concat (file-name-as-directory org-wikish-notes-directory)
          (org-wikish-word-to-filename word)))

(defun org-wikish-word-to-link (word)
  (string-join (list "[[" (org-wikish-word-to-path word) "][" word "]]")))

(defun org-wikish-replace-word-with-link ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (filename (org-wikish-word-to-filename word))
             (link (org-wikish-word-to-link word)))
        (delete-region (car bounds) (cdr bounds))
        (insert (org-wikish-word-to-link word))))))

(defun org-wikish-replace-word-with-link-and-enter ()
  (interactive)
  (org-wikish-replace-word-with-link)
  (org-open-at-point))

(defun org-wikish-page-completion-list ()
  (mapcar (lambda (filename)
            (org-wikish-file-name-to-word filename))
          (remove-if-not (lambda (filename) (string-match "\\.org$" filename))
                         (directory-files org-wikish-notes-directory))))

(defun org-wikish-find-page (name)
  (interactive (list (completing-read "Org file: " (org-wikish-page-completion-list))))
  (org-open-file (org-wikish-word-to-path name)))

(define-key org-mode-map (kbd "C-c w g") #'org-wikish-replace-word-with-link)
(define-key org-mode-map (kbd "C-c w G") #'org-wikish-replace-word-with-link-and-enter)
(define-key global-map (kbd "C-c w f") #'org-wikish-find-page)
