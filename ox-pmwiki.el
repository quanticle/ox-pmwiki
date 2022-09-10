;;; ox-pmwiki.el        -*-lexical-binding: t-*-

;; Copyright (C) 2022 Rohit Patnaik
;; Author: Rohit Patnaik
;; Keywords: org pmwiki
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(require 'ox-md)

(org-export-define-derived-backend 'pmwiki 'md
  :menu-entry 
  '(?w "To pmwiki markup"
        ((?w "To file" (lambda (a s v b) 
                         (org-pmwiki-export-to-pmwiki a s v)))))
  :translate-alist '((bold . org-pmwiki-bold)
                     (italic . org-pmwiki-italic)
                     (underline. org-pmwiki-underline)
                     (strike-through . org-pmwiki-strike-through)
                     (code . org-pmwiki-verbatim)
                     (verbatim . org-pmwiki-verbatim)
                     (paragraph . org-pmwiki-paragraph)
                     (plain-text . org-pmwiki-plain-text)
                     (latex-fragment . org-pmwiki-latex-fragment)
                     (plain-list . org-pmwiki-plain-list)
                     (item . org-pmwiki-item)
                     (template . org-pmwiki-template)
                     (example-block . org-pmwiki-example-block)
                     (src-block . org-pmwiki-example-block)))

(defun org-pmwiki-bold (_bold contents _info)
  "Transcode BOLD object into pmwiki format. 
CONTENTS is the text within the bold markup. INFO is a plist used as a
communication channel."
  (format "\'\'\'%s\'\'\'" contents))

(defun org-pmwiki-italic (_italic contents _info)
  "Transcode ITALIC object into pmwiki format.
CONTENTS is the text within the italic markup. INFO is a plist used as a
communication channel."
  (format "\'\'%s\'\'" contents))

(defun org-pmwiki-underline (_underline contents _info)
  "Transcode UNDERLINE object into pmwiki format.
CONTENTS is the text within the underline markup. INFO is a plist used as a
communication channel."
  (format "{+%s+}" contents))

(defun org-pmwiki-strike-through (_strike-through contents _info)
  "Transcode STRIKE-THROUGH object into pmwiki format.
CONTENTS is the text within the strike-through markup. INFO is a plist used as
a communication channel."
  (format "{-%s-}" contents))

(defun org-pmwiki-verbatim (verbatim _contents _info)
  "Transcode CODE and VERBATIM objects into pmwiki format.
CONTENTS is the text within the markup. INFO is a plist used as a communication
channel."
  (let ((verbatim-text (org-element-property :value verbatim)))
    (format "@@%s@@" verbatim-text)))

(defun org-pmwiki-paragraph (_paragraph contents _info)
  "Transcode PARAGRAPH object into pmwiki format.
CONTENTS is the text of the paragraph. INFO is a plist used as a communication
channel."
  contents)

(defun org-pmwiki-plain-text (text info)
  "Transcode a TEXT string into pmwiki format. 
TEXT is the string to transcode. INFO is a plist holding contextual information."
  (if (plist-get info :with-smart-quotes)
      (org-export-activate-smart-quotes text :utf-8 info)
    text))

(defun org-pmwiki-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT into pmwiki format.
CONTENTS is nil. INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "{$" (substring frag 2 -2) "$}"))
       ((string-match-p "\\\\\\[" frag)
        (concat "{$$" (substring frag 2 -2) "$$}"))
       (t frag)))))

(defun org-pmwiki-plain-list (_plain-list contents _info)
  "Transcode a PLAIN-LIST element into pmwiki format.
CONTENTS is the PLAIN-LIST contents. INFO is a plist used as a communications
channel."
  contents)

(defun org-pmwiki-item (item contents info)
  "Transcode an ITEM into pmwiki format.
CONTENTS is the item contents. INFO is a plist used as a communications
channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (bullet (if (eq type 'ordered) "#" "*")))
    (concat bullet " "
            (pcase (org-element-property :checkbox item)
              (`on "@@[X]@@ ")
              (`off "@@[ ]@@ ")
              (`trans "@@[-]@@ "))
            (let ((tag (org-element-property :tag item)))
              (and tag
                   (format "\'\'\'%s\'\'\'" (org-export-data tag info))))
            (org-trim 
             (and contents 
                  (org-trim 
                   (replace-regexp-in-string "^" "  " contents)))))))

(defun org-pmwiki-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK into pmwiki format.
CONTENTS is nil. INFO is a plist used as a communications channel."
  (concat "[@\n"
          (org-remove-indentation
           (org-export-format-code-default example-block info))
          "@]\n"))

(defun org-pmwiki-template (transcoded-string options)
  "Applies preambles to the final pmwiki output.
TRANSCODED-STRING is the output from all the other transcoders. OPTIONS is a
plist with the export options."
  (concat ""
          (when (plist-get options :title)
            (format "(:title %s :)\n" (org-element-interpret-data (plist-get options :title))))
          (when (plist-get options :with-latex)
            "(:mathjax:)\n")
          transcoded-string))
  

;;;###autoload
(defun org-pmwiki-export-to-pmwiki (&optional async subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".pmwiki" subtreep)))
    (org-export-to-file 'pmwiki outfile async subtreep visible-only)))


(provide 'ox-pmwiki)
;;; ox-pmwiki.el ends here
