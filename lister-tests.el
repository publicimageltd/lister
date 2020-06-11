;;; lister-tests.el --- testsuite for lister.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  

;; Author:  <joerg@joergvolbers.de>
;; Keywords: 

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

;;; Code:

(require 'lister "lister.el")
(require 'buttercup)

(message "Running tests on Emacs %s" emacs-version)

(defun test-buffer () (generate-new-buffer "*LISTER*"))
(defun test-mapper () #'list)
(defun test-buffer-content (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))
(defun test-buffer-content-with-properties (buf)
  (with-current-buffer buf (buffer-string)))
(defun test-point (buf)
  (with-current-buffer buf (point)))

(describe "Core functions on which everything else depends:"
  :var (buf marker)
  (before-each
    (setq buf (test-buffer))
    (setq lister-left-margin 0)
    (setq lister-right-margin 0)
    (setq lister-top-margin 0)
    (setq lister-bottom-margin 0)
    (setq marker (make-marker))
    (set-marker marker (point-max) buf))
  (it "Insert single string at marker position."
    (lister-insert-lines buf marker "TEST")
    (expect (test-buffer-content buf)
	    :to-match
	    "TEST\n"))
  (it "Insert list of strings at marker position."
    (lister-insert-lines buf marker '("1" "2"))
    (expect (test-buffer-content buf)
	    :to-match
	    "1\n2\n")
    (expect (get-text-property marker 'item buf)
	    :to-be
	    t)
    (expect (get-text-property marker 'nlines buf)
	    :to-be
	    2))
  (it "Insert string using using an integer value as position."
    (lister-insert-lines buf (marker-position marker) "TEST")
    (expect (test-buffer-content buf)
	    :to-match
	    "TEST\n")))

(describe "Inserting header, footer and items:"
  :var (buf header footer)
  (before-each
    (setq buf (lister-setup (test-buffer)
			    (test-mapper)))
    (setq header "HEADER")
    (setq footer "FOOTER")
    (setq data   "DATA")
    (setq lister-left-margin 0)
    (setq lister-right-margin 0)
    (setq lister-top-margin 0)
    (setq lister-bottom-margin 0))
  (it "Insert a single header."
    (lister-set-header buf header)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n")))
  (it "Insert a single footer."
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat footer "\n")))
  (it "Insert header and a footer."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n" footer "\n")))
  (it "Insert header and footer with left margin."
    (setq lister-left-margin 3)
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat (make-string lister-left-margin ? )
		    header
		    "\n"
		    (make-string lister-left-margin ? )
		    footer
		    "\n")))
  (it "Insert header and footer with top margin."
    (setq lister-top-margin 1)
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat "\n" header "\n" "\n" footer "\n")))
  (it "Insert header and footer, then remove footer."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-set-footer buf nil)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n")))
  (it "Insert header and footer, then removee header."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-set-header buf nil)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat footer "\n")))
  (it "Add header and footer, then add a data item."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-add buf data)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n" data "\n" footer "\n")))
  (it "Add header and footer, add data item, remove footer."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-add buf data)
    (lister-set-footer buf nil)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n" data "\n")))
  (it "Add header and footer, add data item, remove header."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-add buf data)
    (lister-set-header buf nil)
    (expect (test-buffer-content buf)
	    :to-match
	    (concat data "\n" footer "\n")))
  (it "Add header, footer, some list items."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-add buf "1")
    (lister-add buf "2")
    (lister-add buf "3")
    (expect (test-buffer-content buf)
	    :to-match
	    (concat header "\n1\n2\n3\n" footer "\n")))
  (it "Add header, footer, list items, remove all using index 0."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (lister-add buf "1")
    (lister-add buf "2")
    (lister-add buf "3")
    (lister-remove buf 0)
    (lister-remove buf 0)
    (lister-remove buf 0)
    (lister-set-header buf nil)
    (lister-set-footer buf nil)
    (expect (test-buffer-content buf)
	    :to-match
	    ""))
  (it "Add list items, remove them all using index 0."
    (lister-add buf "1")
    (lister-add buf "2")
    (lister-add buf "3")
    (lister-remove buf 0)
    (lister-remove buf 0)
    (lister-remove buf 0)
    (expect (test-buffer-content buf)
	    :to-match
	    "")))

(describe "Cursor motion (lister-goto):"
  :var (buf header footer)
 (before-each
   (setq buf (lister-setup (test-buffer)
			   (test-mapper)))
   (setq header "HEADER")
   (setq footer "FOOTER")
   (lister-add buf "1")
   (lister-add buf "2")
   (lister-add buf "3"))
 (it "Move to the first item with :first."
   (lister-goto buf :first)
   (expect (test-point buf)
	   :to-be
	   1))
 (it "Move to the last item with :last."
   (lister-goto buf :last)
   (expect (test-point buf)
	   :to-be
	   5))
 (it "Move to the middle item, using index 1."
   (lister-goto buf 1)
   (expect (test-point buf)
	   :to-be
	   3))
 (it "Remove middle item, go to last item using :last."
   (lister-remove buf 1)
   (lister-goto buf :last)
   (expect (test-point buf)
	   :to-be
	   3))
 (it "Add header, move to the first item with :first."
   (lister-set-header buf header)
   (lister-goto buf :first)
   (expect (test-point buf)
	   :to-be
	   (+ 1 (length header) 1)))
 (it "Add header, move to the last item with :last."
   (lister-set-header buf header)
   (lister-goto buf :last)
   (expect (test-point buf)
	   :to-be
	   (+ 5 (length header) 1)))
 (it "Add header, move to the middle item, using index 1."
   (lister-set-header buf header)
   (lister-goto buf 1)
   (expect (test-point buf)
	   :to-be
	   (+ 3 (length header) 1)))
 (it "Add header, remove middle item, go to last item using :last."
   (lister-set-header buf header)
   (lister-remove buf 1)
   (lister-goto buf :last)
   (expect (test-point buf)
	   :to-be
	   (+ 3 (length header) 1))))

(describe "Using predicates:"
  :var (buf datalist)
  (before-each
    (setq datalist '("AA" "AB" "BA" "BB"))
    (setq buf (lister-setup (test-buffer)
			    (test-mapper)
			    datalist))
    (setq lister-left-margin 0)
    (setq lister-right-margin 0)
    (setq lister-top-margin 0)
    (setq lister-bottom-margin 0))
 (it "Local copy of data is correct."
   (expect (with-current-buffer buf
	     lister-local-data-list
	     :to-equal
	     datalist)))
 (it "Generated copy of data is correct."
   (expect (lister-get-all-data buf)
	   :to-equal
	   datalist))
 (it "Filter all data so that nothing is displayed."
   (lister-add-filter buf (lambda (data) nil))
   (expect (test-buffer-content buf)
	   :to-match
	   ""))
 (it "Apply filter which matches only some data."
   (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
   (expect (lister-get-all-data buf)
	   :to-equal
	   '("AA" "AB")))
 (it "Apply filter chain."
   (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
   (lister-add-filter buf (lambda (data) (string-match-p "B" data)))
   (expect (lister-get-all-data buf)
	   :to-equal
	   '("AB"))))

(describe "Using the mapper fn:"
  :var (datalist)
  (before-each
    (setq datalist '(1 2 3 4 nil 5)))
  (it "Do not display nil items returned by the mapper."
    (let* ((buf (lister-setup (test-buffer)
			      (lambda (data)
				(when data 
				  (format "%d" data)))
			      datalist)))
      (expect (lister-get-all-data buf)
	      :to-equal
	      '(1 2 3 4 5)))))

(describe "Use a callback function:"
  :var (value buf)
  (before-each
    (setq buf (lister-setup (test-buffer)
			    #'list
			    '("A" "B" "C" "D")))
    (switch-to-buffer buf) ;; we need the window to be set.
    (setq value nil))
  (it "Call the callback when entering item."
    (lister-add-enter-callback buf
			       (lambda ()
				 (setq value
				       (lister-get-data
					buf
					:point))))
    (lister-goto buf :last)
    (expect value :to-match "D"))
  (it "Call the callback when leaving item."
    (lister-add-leave-callback buf
			       (lambda ()
				 (setq value
				       (lister-get-data
					buf
					:point))))
    (lister-goto buf :last)
    (lister-goto buf :first)
    (expect value :to-match "D")))


(provide 'lister-tests)
;;; lister-tests.el ends here
