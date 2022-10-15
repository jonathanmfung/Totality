;;; Setup
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

;;; Utilities

(defun jf/denote-file-to-link (file)
  "Convert a FILE path to link string."
  (let* ((formatter (denote-link--file-type-format file nil)))
    (denote-link--format-link file formatter)))

(defun jf/denote-get-backlinks-as-links (file &optional as-file)
  "Retrieve all file paths that link top FILE."
  (when (denote-file-is-writable-and-supported-p file)
    (let ((id (denote-retrieve-filename-identifier file)))
      ;; denote--retrieve-process-grep pulls all backlinks
      (if-let ((backlink-files
		(seq-filter
		 #'denote-file-is-note-p
		 (delete file (denote--retrieve-files-in-xrefs
			       (denote--retrieve-xrefs id))))))
	  (if as-file
	      backlink-files
	    (mapcar 'jf/denote-file-to-link backlink-files))
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

;;; Graph Building

(defun jf/denote-get-id (file)
  "Retrieve the ID of a FILE, usually the timestamp."
  (substring
   (remove ?T (file-name-nondirectory file))
   0 14))

(defun jf/denote-get-title (file)
  "Get the proper title of a FILE."
  (substring (jf/denote-file-to-link file) 26 -2))

(jf/denote-get-id "/home/jonat/denote-notes/20221005T172646--s-expression__draft.org")
(jf/denote-get-title "/home/jonat/denote-notes/20221005T172646--s-expression__draft.org")

(defun jf/denote-pair-to-dot (pair)
  "(car PAIR) is \"/abs/file.org\"
(con PAIR) is (\"/abs/back1.org\" \"/abs/back2\") or '(\"No backlinks to the current note\")
"
  (let* ((file (car pair))
	 (file-id (jf/denote-get-id file))
	 (file-title (jf/denote-get-title file))
	 (file-url (file-name-with-extension (file-name-nondirectory file) "html"))
	 (node-info (format "%s [label=\"%s\" URL=\"%s\"];\n" file-id file-title file-url))
	 (blinks (cadr pair))
	 (backlinks (if (equal blinks '("No backlinks to the current note"))
			""
		      (mapconcat (lambda (b) (format "%s -> %s;" (jf/denote-get-id b) file-id ))
				 blinks "\n"))))
    (concat node-info backlinks)))

;; (jf/denote-pair-to-dot
;;  '("/home/jonat/denote-notes/20221005T172646--s-expression__draft.org"
;;    ("/home/jonat/denote-notes/20221005T172259--webassembly-wasm__programming-language-theory_virtual-machine.org"
;;     "/home/jonat/denote-notes/20221012T170159--lisp__programming-language-theory.org")))
;; (jf/denote-pair-to-dot
;;  '("/home/jonat/denote-notes/20221005T172646--s-expression__draft.org"
;;    ("No backlinks to the current note")))


;; cannot use denote-link-find-file, because it needs to search a buffer for link
(defun jf/denote-graph-string ()
  (let* ((files (jf/denote-all-files))
	 (backlinks-files (mapcar (lambda (f) (jf/denote-get-backlinks-as-links f t)) files))
	 (zipped (cl-mapcar (lambda (f bs) (cons f (list bs))) files backlinks-files))
	 (f-title (mapcar 'jf/denote-get-title files))
	 (dot-info (string-join (mapcar 'jf/denote-pair-to-dot zipped) "\n"))
	 (dot-info-full (concat "digraph {\n  ratio = 1.0\n" dot-info "\n}"))
	 (temp-dot (make-temp-file "graph" nil ".dot" dot-info-full)))
    ;; dot generates some header meta-data such as <?xml> and <!DOCTYPE>, so cut away with sed.
    (shell-command-to-string (concat "fdp -Tsvg " temp-dot " | sed '1,6d'"))))


;;; Export Generation

(defun jf/denote-export-create-index (files)
  (let* ((links (mapcar #'jf/denote-file-to-link files))
	 (path (file-name-concat denote-directory "index.html")))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: Totality: Index\n")
      (insert (format "#+EXPORT_FILE_NAME: %s" path))
      (insert (format "\n#+BEGIN_EXPORT html\n %s \n#+END_EXPORT\n" (jf/denote-graph-string)))
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
	(let ((backlinks (jf/denote-get-backlinks-as-links file)))
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
	(dir-html (file-name-concat denote-directory "docs/")))
    (files--ensure-directory dir-html)
    (shell-command (concat "mv " files-html " " dir-html))))

;; NOTE Need to remove all files in docs/, then process
;; since renaming files (e.g. changing tags) leads to multiple FILE.html
