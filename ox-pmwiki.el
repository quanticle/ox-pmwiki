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

(defgroup org-export-pmwiki nil
  "Options specific to PMWiki export back-end."
  :tag "Org PMWiki"
  :group 'org-export
  :version "28.1"
  :package-version '(ox-pmwiki . "0.0.1"))

(defcustom org-pmwiki-toplevel-hlevel 2
  "Heading level to use for level 1 Org headings in pmwiki export.

If this is 1, headline levels will be preserved on export. If this is 2, top
level Org headings will be exported to level 2 headings, level 2 Org headings
will be exported to level 3 headings, and so on.

The default value for this variable is 2 because PMWiki styles
document titles with H1 by default."
  :group 'org-export-pmwiki
  :type 'integer)

(defcustom org-pmwiki-inline-image-rules
  `(("file" . ,(regexp-opt '(".jpg" ".png" ".gif")))
    ("http" . ,(regexp-opt '(".jpg" ".png" ".gif")))
    ("https" . ,(regexp-opt '(".jpg" ".png" ".gif"))))
  "Rules characterizing image files that can be inlined into PMWiki markup.
A rule consists of an association whose key is the type of link to consider and
value is a regexp that will be matched against the link's path. Note that PMWiki
will automatically inline images if a HTTP(S) link points to an image file, of
type `.jpg', `.png', or `.gif', so this setting only determines if file links
get inlined. However, it is still used by e.g. ORG-PMWIKI-INLINE-IMAGE-P to
determine if a given link is an image link.

Also note that this just tells ox-pmwiki which files to inline as attachments in
the markup. It's still up to you to actually upload those files to your wiki to
ensure that links aren't broken."
  :group 'org-export-pmwiki
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

(org-export-define-derived-backend 'pmwiki 'md
  :menu-entry
  '(?w "Export to pmwiki markup"
        ((?W "To temporary buffer"
             (lambda (a s v b) (org-pmwiki-export-as-pmwiki a s v)))
         (?w "To file" (lambda (a s v b)
                         (org-pmwiki-export-to-pmwiki a s v)))))
  :translate-alist '((bold . org-pmwiki-bold)
                     (italic . org-pmwiki-italic)
                     (underline . org-pmwiki-underline)
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
                     (src-block . org-pmwiki-example-block)
                     (headline . org-pmwiki-headline)
                     (inner-template . org-pmwiki-inner-template)
                     (link . org-pmwiki-link)
                     (quote-block . org-pmwiki-quote-block)
                     (superscript . org-pmwiki-superscript)
                     (line-break . org-pmwiki-line-break)
                     (table . org-pmwiki-table)
                     (table-cell . org-pmwiki-table-cell)
                     (table-row . org-pmwiki-table-row)
                     (subscript . org-pmwiki-subscript)
                     (superscript . org-pmwiki-superscript))
  :options-alist '((:pmwiki-toplevel-hlevel nil nil org-pmwiki-toplevel-hlevel)
                   (:pmwiki-inline-image-rules nil nil org-pmwiki-inline-image-rules)))

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
    (format "@@[=%s=]@@" verbatim-text)))

(defun org-pmwiki-inline-image-p (link info)
  "Non-nil when LINK is meant to appear as an image.
INFO is a plist used as a communication channel. LINK is an inline image when
it has no description and targets an image file, (see
`org-pmwiki-inline-image-rules' for more information), or if its description is
a single link targeting an image file.."
  (if (not (org-element-contents link))
      (org-export-inline-image-p link (plist-get info :pmwiki-inline-image-rules))
    (not
     (let ((link-count 0))
       (org-element-map (org-element-contents link)
           (cons 'plain-text org-element-all-objects)
         (lambda (obj)
           (pcase (org-element-type obj)
             (`plain-text (org-string-nw-p obj))
             (`link (if (= link-count 1) t
                      (cl-incf link-count)
                      (not (org-export-inline-image-p obj (plist-get info :pmwiki-inline-image-rules)))))
             (_ t)))
         info t)))))

(defun org-pmwiki-standalone-image-p (paragraph info)
  "Determine if a paragraph is a standalone image. A paragraph is a standalone
image if its sole contents is a link to an image file."
  (catch 'exit
    (let ((link-count 0))
      (org-element-map (org-element-contents paragraph)
          (cons 'plain-text org-element-all-objects)
        (lambda (obj)
          (when (pcase (org-element-type obj)
                  (`plain-text (org-string-nw-p obj))
                  (`link (or (> (cl-incf link-count) 1)
                             (not (org-pmwiki-inline-image-p obj info))))
                  (_ t))
            (throw 'exit nil)))
        info nil 'link)
      (= link-count 1))))

(defun org-pmwiki--wrap-image (contents &optional caption)
  (format "%%cframe%%%s%s"
          (org-trim contents)
          (if caption
              (format " | %s" caption)
            "")))

(defun org-pmwiki-paragraph (paragraph contents info)
  "Transcode PARAGRAPH object into pmwiki format.
CONTENTS is the text of the paragraph. INFO is a plist used as a communication
channel."
  (cond
   ((org-pmwiki-standalone-image-p paragraph info)
    (let ((raw-caption (org-export-data (org-export-get-caption paragraph) info)))
      (if (and raw-caption (org-string-nw-p raw-caption))
          (org-pmwiki--wrap-image contents raw-caption)
        (org-pmwiki--wrap-image contents))))
   (t contents)))

(defun org-pmwiki-plain-text (text info)
  "Transcode a TEXT string into pmwiki format.
TEXT is the string to transcode. INFO is a plist holding contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  text)

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
         (bullet (if (eq type 'ordered) "#" "*"))
         (counter (org-element-property :counter item)))
    (concat bullet " "
            (if counter
                (format "%%item value=%s%%" counter)
              "")
            (pcase (org-element-property :checkbox item)
              (`on "@@[X]@@ ")
              (`off "@@[ ]@@ ")
              (`trans "@@[-]@@ "))
            (let ((tag (org-element-property :tag item)))
              (and tag
                   (format "\'\'\'%s\'\'\'" (org-export-data tag info))))
            (org-trim
             (or (and contents
                      (org-trim
                       (replace-regexp-in-string "^" "  " contents)))
                 "")))))

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
          (when (plist-get options :with-toc)
            (let* ((toplevel-hlevel (plist-get options :pmwiki-toplevel-hlevel))
                  (toc-max (plist-get options :with-toc))
                  (toc-level (min 6 (+ toplevel-hlevel (1- toc-max)))))
              (format "(:htoc end=%d:)\n" toc-level)))
          transcoded-string))

(defun org-pmwiki-inner-template (contents _info)
  "Return the body of the document, to prevent Markdown's default TOC
generation"
  contents)

(defun org-pmwiki-headline (headline contents info)
  "Transcode a HEADLINE element into pmwiki format.
CONTENTS is the headline contents. INFO is a plist used as a communications
channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (+ (org-export-get-relative-level headline info)
                     (1- (plist-get info :pmwiki-toplevel-hlevel))))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list (concat "  " (org-make-tag-string tag-list))))))
           (priority (and (plist-get info :with-priority)
                          (let ((char (org-element-property :priority headline)))
                            (and char (format "[#%c] " char)))))
           (heading (concat todo priority title)))
      (cond
       ((or (org-export-low-level-p headline info)
            (> level 6))
        (let ((bullet (if (not (org-export-numbered-headline-p headline info)) "*" "#")))
          (concat bullet " " "\'\'\'" heading tags "\'\'\'" "\n"
                  (and contents (replace-regexp-in-string "^" "  " contents)))))
       (t
        (let ((level-mark (make-string level ?!)))
          (concat "\n" level-mark " " heading tags "\n\n" contents)))))))

(defun org-pmwiki-link (link desc info)
  "Transcode a LINK element into pmwiki format.
DESC is the description part of the link, or the empty string. INFO is a plist
used as a communications channel.

Note that, for now, only internet (i.e. http(s), ftp, mailto) and file links are
supported. ox-pmwiki assumes that if the user links to an org-file, they will
also be uploading that file as a wiki page with the page name as the file name,
and thus translates file links into wikilinks to the file name. If the file link
points to an image (of type `.png`, `.jpg`, or `.gif`), the filename is
converted into an attachment of the same name. In addition, line breaks in link
descriptions are removed, because PMWiki does not support links that span
multiple lines."
  (let ((type (org-element-property :type link))
        (raw-path (org-element-property :path link))
        (fixed-desc (if (and desc (not (string= "" (org-trim desc))))
                        (replace-regexp-in-string "\n" " " desc)
                      desc)))
    (cond
     ((member type '("http" "https" "ftp" "mailto"))
      (let ((encoded-path (url-encode-url (concat type ":" raw-path))))
        (if fixed-desc
            (format "[[%s | %s]]" encoded-path fixed-desc)
          encoded-path)))
     ((string= type "file")
      (cond
       ((string= (file-name-extension raw-path) "org")
        (let ((wiki-page-name (file-name-sans-extension
                               (file-name-nondirectory raw-path))))
          (if fixed-desc
              (format "[[%s | %s]]" wiki-page-name fixed-desc)
            (format "[[%s]]" wiki-page-name))))
       ((member (file-name-extension raw-path) '("jpg" "png" "gif"))
        (let ((attachment-name (file-name-nondirectory raw-path)))
          (if desc
              (format "[[Attach:%s | %s]]" attachment-name desc)
            (format "Attach:%s" attachment-name)))))))))

(defun org-pmwiki-quote-block (_quote-block contents _info)
  "Transcode a QUOTE-BLOCK element into pmwiki format.
CONTENTS is the contents of the quote block. INFO is a plist used as a
communications channel."
  (replace-regexp-in-string
   "^" "->"
   (replace-regexp-in-string "\n\\'" "" contents)))

(defun org-pmwiki-superscript (_superscript contents _info)
  "Transcode superscript text into pmwiki format.
CONTENTS is the contents of the superscript element. INFO is a plist used as a
communications channel."
  (format "'^%s^'" contents))

(defun org-pmwiki-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK from org to pmwiki format.
CONTENTS is nil. INFO is a plist holding contextual information."
  "\\\\\n")

(defun org-pmwiki-table (table contents info)
  "Transcode a TABLE element from org to pmwiki format. CONTENTS is the contents
of the table. INFO is a plist serving as a communications channel."
  (let ((table-border "border=1")
        (table-cell-padding "cellpadding=5px"))
    (format "||%s %s\n%s"
            table-border
            table-cell-padding
            contents)))

(defun org-pmwiki-table-row (_table-row contents _info)
  "Transcode a TABLE-ROW element from org to pmwiki format. CONTENTS is the
contents of the row."
  (if (and contents (not (string= "" (org-trim contents)))) 
      (format "%s\n" contents)))

(defun org-pmwiki-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from org to pmwiki format. CONTENTS is the
contents of the cell. INFO is a PLIST serving as a communications channel."
  (let* ((table-row (org-element-parent table-cell))
         (table (org-element-lineage table-cell 'table))
         (cell-alignment (org-export-table-cell-alignment table-cell info))
         (cell-text (if (or (not contents) (string= "" (org-trim contents)))
                        "&nbsp;"
                      contents))
         (cell-contents 
          (cond ((eq 'left cell-alignment) (format "%s " cell-text))
                ((eq 'center cell-alignment) (format " %s " cell-text))
                ((eq 'right cell-alignment) (format " %s" cell-text)))))
    (if (and (org-export-table-has-header-p table info)
             (= 1 (org-export-table-row-group table-row info)))
        (if (zerop (cdr (org-export-table-cell-address table-cell info)))
            (format "||!%s||" cell-contents)
          (format "!%s||" cell-contents))
      (if (zerop (cdr (org-export-table-cell-address table-cell info)))
          (format "||%s||" cell-contents)
        (format "%s||" cell-contents)))))

(defun org-pmwiki-superscript (_superscript contents _info)
  "Transcode a SUPERSCRIPT object from org to pmwiki format. CONTENTS is the
superscript text. INFO is a plist holding contextual information."
  (format "\'^%s^\'" contents))

(defun org-pmwiki-subscript (_subscript contents _info)
  "Transcode a SUBSCRIPT object from org to pmwiki format. CONTENTS is the
subscript text. INFO is a plist holding contextual information."
  (format "\'_%s_\'" contents))

;;;###autoload
(defun org-pmwiki-export-to-pmwiki (&optional async subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".pmwiki" subtreep)))
    (org-export-to-file 'pmwiki outfile async subtreep visible-only)))

;;;###autoload
(defun org-pmwiki-export-as-pmwiki (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'pmwiki "*Org pmwiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


(provide 'ox-pmwiki)
;;; ox-pmwiki.el ends here
