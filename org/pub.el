#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'ox)
(require 'ox-html)
(load (expand-file-name "elpa/htmlize-1.57/htmlize.el" user-emacs-directory) t t)
(require 'htmlize)

(setq
 org-html-style-default ""
 org-html-scripts ""
 org-html-htmlize-output-type 'css
 org-html-doctype "html5"
 org-html-html5-fancy t
 org-html-validation-link nil
 org-html-postamble t
 make-backup-files nil
 debug-on-error t)
