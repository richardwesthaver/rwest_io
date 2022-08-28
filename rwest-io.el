;;; rwest-io.el --- rwest.io emacs apps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: convenience, internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the source code for building the static content of my
;; website: <https://rwest.io>.

;;; Code:
(require 'ox-publish)
(require 'org-id)

;; if running emacs -Q/q/batch
(require 'htmlize (expand-file-name "elpa/htmlize-20210825.2150/htmlize.el" user-emacs-directory))

;; (require 'htmlize)

(defgroup rwest-io nil
  "Customization group for rwest_io Emacs apps
URL `https://rwest.io'")

;;; custom
;; 
;; change me!
(defcustom rwest-io-project-dir "~/dev/rwest_io"
  "location of `rwest-io' project source code"
  :type 'directory)

(defcustom rwest-io-publish-dir "/sshx:hyde:/var/www/rwest.io"
  "publish `rwest-io' files to this directory"
  :type 'directory
  :group 'rwest-io)

(defcustom rwest-io-sitemap-entry-format 'org-sitemap-entry-format
  "function to be used for sitemap entries. must accept 3 params:
entry style and project"
  :group 'rwest-io
  :type 'function)

(defcustom rwest-io-theme-path (concat rwest-io-project-dir "/org/clean.theme")
  "path to the SETUPFILE that will be used"
  :type 'file
  :group 'rwest-io)

(defcustom rwest-io-url "https://rwest.io"
  "URL of my website"
  :type 'url
  :group 'rwest-io)

;;; local

(defmacro rw-path (path)
  "concat PATH on `rwest-io-project-dir'"
  (concat rwest-io-project-dir "/" path))

(defmacro rw-pub-path (path)
  "concat PATH on `rwest-io-publish-dir'"
  (concat rwest-io-publish-dir path))

;; local dependencies
(require 'elsrv (rw-path "el/elsrv.el"))

;; publish config
(setq org-html-style-default ""
      org-html-scripts ""
      org-html-htmlize-output-type 'css
      org-export-htmlize-output-type 'css
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-src-fontify-natively t
      make-backup-files nil
      debug-on-error t)

(setq org-publish-project-alist
      `(("content"
	 :base-directory "org"
	 :base-extension "org"
	 :recursive nil
	 :htmlized-source t
	 :html-preamble "<nav><a href = \"./\">home</a> | <a href = \"./blog.html\">blog</a> | <a href = \"./notes.html\">notes</a> | <a href = \"./projects.html\">projects</a></nav>"
	 :publishing-directory ,rwest-io-publish-dir
	 :org-publish-use-timestamps-flag nil
	 :publishing-function org-html-publish-to-html)
	("blog"
	 :base-directory "org/blog"
	 :base-extension "org"
	 :publishing-directory ,(concat rwest-io-publish-dir "/blog")
	 :org-publish-use-timestamps-flag nil
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-preamble "<nav><a href = \"../\">home</a></nav>")
	("notes"
	 :base-directory "org/notes"
	 :base-extension "org"
	 :publishing-directory ,(concat rwest-io-publish-dir "/notes")
	 :org-publish-use-timestamps-flag nil
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-preamble "<nav><a href = \"../\">home</a> | <a</nav>")
	("projects"
	 :base-directory "org/projects"
	 :base-extension "org"
	 :publishing-directory ,(concat rwest-io-publish-dir "/projects")
	 :org-publish-use-timestamps-flag nil
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-preamble "<nav><a href = \"../\">home</a></nav>")
	("media"
	 :base-directory "org/media"
	 :base-extension "css\\|txt\\|jpg\\|jpeg\\|gif\\|png\\|mp3\\|wav\\|flac\\|ogg\\|mp4"
	 :recursive t
	 :publishing-directory ,(concat rwest-io-publish-dir "/media")
	 :publishing-function org-publish-attachment)
	("static"
	 :base-directory "static"
	 :base-extension "css\\|txt\\|jpg\\|gif\\|png"
	 :recursive t
	 :publishing-directory ,rwest-io-publish-dir
	 :publishing-function org-publish-attachment)
	;; be aware, the ordering of components matters..
	("rwest.io" :components ("blog" "notes" "projects" "content" "static"))
	("rwest.io-with-static" :components ( "blog" "notes" "projects" "content" "static" "media"))))

;;; org-id utils

(defun org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
(org-with-point-at pom
  (let ((id (org-entry-get nil "CUSTOM_ID"))
	;; use CUSTOM_ID for links
	(org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "org")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun org-id-add-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-custom-id-get (point) 'create))))

;;; postamble

(setq org-html-postamble "<footer><div><p>created %d;<br>updated %C;</p></div></footer>")

;;; commands

;;;###autoload
(defun rwest-io-publish (&optional sitemap static force)
  "publish `rwest-io' content.
If SITEMAP is t, also generate new sitemap.org files.
If STATIC is t, also publish media and static files.
If FORCE is t, skip checking file mod date and just publish all files.
If given a prefix (C-u), set all args to t"
  (interactive)
  (if current-prefix-arg
      (setq sitemap t
	    static t
	    force t))
  (let ((default-directory rwest-io-project-dir)
	(prj-name (if static "rwest.io-with-static"
		    "rwest.io")))
    (message (format "publishing from %s" default-directory))    
    (if sitemap (rwest-io-update-sitemap))
    (org-publish prj-name force)))

;;;###autoload
(defun rwest-io-update-sitemap ()
  "update `rwest-io' sitemaps:
- blog
- notes
- projects"
  (interactive)
  (let ((dirs '("blog" "notes" "projects")))
    (message (format "generating sitemaps: %s" dirs))
    (while dirs
      (let* ((dir (pop dirs))
	     (default-directory (concat rwest-io-project-dir "/org/" dir))
	     (files (directory-files default-directory nil ".org$" t))
	     (entries))
	(delete "sitemap.org" files)
	(while files
	  (let* ((file (pop files)))
	    (with-temp-buffer
	      (insert-file-contents file nil)
	      (add-to-list
	       'entries
	       (cons file (org-collect-keywords '("TITLE" "DATE")))))))
	(sort entries
	      (lambda (a b)
		(string-greaterp (car (cdaddr a)) (car (cdaddr b)))))
	(with-temp-file "sitemap.org"
	  (insert (format "#+TITLE: %s\n" dir))
	  (dolist (e entries)
	    (let ((file (file-name-with-extension (car e) "html"))
		  (title (cadadr e))
		  (date (car (cdaddr e))))
	      (insert (format "- %s [[%s/%s/%s][%s]]\n" date rwest-io-url dir file title)))))
	(message (format "generated org/%s/sitemap.org" dir))))))

(provide 'rwest-io)
;;; rwest-io.el ends here
