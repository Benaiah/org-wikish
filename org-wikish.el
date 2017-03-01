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

(defun org-wikish-file-name (path)
  (concat (file-name-base path) (file-name-extension path)))

(defun org-wikish-maybe-relative-path (path &optional buffer-path)
  (let ((buffer-path (if buffer-path buffer-path buffer-file-name)))
    (if (and (file-name-absolute-p path)
             buffer-path
             (org-wikish-in-wiki-p buffer-path))
        (let ((newpath (file-relative-name path org-wikish-notes-directory)))
          (concat "./" newpath))
      path)))

(defun org-wikish-word-to-path (word)
  (org-wikish-maybe-relative-path
   (concat (file-name-as-directory org-wikish-notes-directory)
           (org-wikish-word-to-filename word))))

(defun org-wikish-word-to-link (word)
  (string-join (list "[[" (org-wikish-word-to-path word) "][" word "]]")))

(defun org-wikish-in-wiki-p (path)
  (string-equal
   (file-name-directory path)
   (file-name-as-directory org-wikish-notes-directory)))

(defun org-wikish-link-word-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((word (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (filename (org-wikish-word-to-filename word))
             (link (org-wikish-word-to-link word)))
        (delete-region (car bounds) (cdr bounds))
        (insert link)))))

(defun org-wikish-ensure-page-exists (word)
  (let ((path (org-wikish-word-to-path word)))
    (if (not (file-exists-p path))
        (write-region "" nil path))))

(defun org-wikish-link-word-at-point-and-enter (&optional dont-create)
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (org-wikish-link-word-at-point)
    (if (not dont-create)
        (org-wikish-ensure-page-exists word))
    (org-open-at-point)))

(defun org-wikish-open-link-at-point ()
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (org-open-at-point)
    (org-wikish-link-word-at-point-and-enter)))

(defun org-wikish-page-completion-list ()
  (mapcar (lambda (filename)
            (org-wikish-file-name-to-word filename))
          (remove-if-not (lambda (filename) (string-match "\\.org$" filename))
                         (directory-files org-wikish-notes-directory))))

(defun org-wikish-find-page (name)
  (interactive (list (completing-read "Org file: " (org-wikish-page-completion-list))))
  (org-open-file (org-wikish-word-to-path name)))

(define-key org-mode-map (kbd "C-c w g") #'org-wikish-link-word-at-point)
(define-key org-mode-map (kbd "C-c w G") #'org-wikish-link-word-at-point-and-enter)
(define-key org-mode-map (kbd "C-c w o") #'org-wikish-open-link-at-point)
(define-key global-map (kbd "C-c w f") #'org-wikish-find-page)

