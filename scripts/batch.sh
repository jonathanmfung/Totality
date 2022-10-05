#!/bin/sh
# Tangle files with Org mode
#
emacs -Q --batch -l ~/denote-notes/scripts/html-export.el --eval "
    (progn
	(require 'ox-html)
	(jf/denote-export-directory-backlinks)
	(jf/denote-move-htmls)
    )
  "
