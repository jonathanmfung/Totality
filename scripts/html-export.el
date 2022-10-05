(require 'package)
(package-initialize)

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package denote
  :config
  (setq denote-file-type 'org 		; same as default
	denote-directory (substitute-in-file-name "$HOME/denote-notes")
	denote-dired-directories (list denote-directory)
	denote-prompts '(title template keywords)
	)
  (setq denote-templates '((reference . "TEXT\n\n\n* Resources\n - LINK")))
  (setq xref-search-program 'ripgrep)	; denote-link-backlinks uses xref

  :bind (("C-c n l" . #'denote-link-or-create) ; safe link insertion
	 ("C-c n f" . #'denote-open-or-create) ; safe note creation
	 ("C-c n b" . #'denote-link-backlinks)
	 ("C-c n s" . #'denote-link-find-file) ; open links in buffer
	 ("C-c n r" . #'prot-dired-limit-regexp)
	 ("C-c n c" . #'denote-rename-file-using-front-matter) ; update file name when modifying front-matter
	 )
  :hook
  (dired-mode . denote-dired-mode-in-directories))

(require 'denote)

(defun jf/denote-file-to-link (file)
  "Convert a FILE path to link string."
  (let* ((formatter (denote-link--file-type-format file nil)))
    (denote-link--format-link file formatter)))

(defun jf/denote-get-backlinks-as-links (file)
  "Retrieve all file paths that link top FILE."
  (when (denote-file-is-writable-and-supported-p file)
    (let ((id (denote-retrieve-filename-identifier file)))
      ;; denote--retrieve-process-grep pulls all backlinks
      (if-let ((backlink-files
		(seq-filter
		 #'denote-file-is-note-p
		 (delete file (denote--retrieve-files-in-xrefs
			       (denote--retrieve-xrefs id))))))
	  (mapcar 'jf/denote-file-to-link backlink-files)
        '("No backlinks to the current note")))))

(defun denote-link-ol-export (link description format)
  "Export a `denote:' link from Org files.
The LINK, DESCRIPTION, and FORMAT are handled by the export
backend.

Modified for use in html export, to remove hardcoded 'target=\"_blank\"'.
"
  (let* ((path-id (denote-link--ol-resolve-link-to-target link :path-id))
         (path (file-name-nondirectory (car path-id)))
         (p (file-name-sans-extension path))
         (id (cdr path-id))
         (desc (or description (concat "denote:" id))))
    (format "<a href=\"%s.html\">%s</a>" p desc)))

(defun jf/denote-all-files ()
  (seq-filter
   (lambda (f) (and
		(string= (file-name-extension f) "org")
		(denote-file-is-note-p f)))
   (directory-files denote-directory t)))

(defun jf/denote-export-create-index (files)
  (let* ((links (mapcar #'jf/denote-file-to-link files))
	 (path (file-name-concat denote-directory "index.html")))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Index\n")
      (insert (format "#+EXPORT_FILE_NAME: %s" path))
      (insert (mapconcat (lambda (x) (format "\n- %s \n" x)) links))
      (org-html-export-to-html))))

(defun jf/denote-export-directory-backlinks ()
  "Export all denote org files to html, with backlinks section."
  (let
      ((default-directory denote-directory)
       (files (jf/denote-all-files))
       (org-export-with-toc nil)
       (org-html-postamble nil))
    (progn
      (jf/denote-export-create-index files)
      (dolist (file files)
	(let ((backlinks (jf/denote-get-backlinks-as-links file))
	      )
	  (with-current-buffer
	      (find-file-noselect file)
	    (save-excursion
	      (goto-char (point-max))
	      (insert "\n\n* Backlinks\n")
	      ;; to remove target=_blank, need to modify denote-link-ol-export,
	      ;; which is inspired by org-man-export
	      (insert (mapconcat (lambda (x) (format "\n- %s \n" x)) backlinks))
	      (insert "\n\n* [[./index.html][Index]]\n")
	      (org-html-export-to-html)
	      (revert-buffer nil t))))))))

(defun jf/denote-move-htmls ()
  "Equivalent to > mv ~/denote-notes/*.html ~/denote-notes/html."
  (let ((files-html (file-name-concat denote-directory "*.html"))
	(dir-html (file-name-concat denote-directory "html/")))
    (files--ensure-directory dir-html)
    (shell-command (concat "mv " files-html " " dir-html))))

;; NOTE Need to remove all files in html/, then process
;; since renaming files (e.g. changing tags) leads to multiple FILE.html

;; TODO graphs
;; cat foo.dot
;; digraph {
;; 	A [URL="https://google.com"];
;; 	A -> B;
;; 	A -> C;
;; 	C -> B;
;; }
;; > dot -Tsvg foo.dot > foo.svg
;; use make-temp-file, reference from org-roam-graph
