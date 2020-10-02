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

;; * Functions for common setup tasks
;;
(defun test-new-buffer ()
  (generate-new-buffer "*LISTER*"))

(defun test-buffer-content (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun test-buffer-content-with-properties (buf)
  (with-current-buffer buf (buffer-string)))

(defun test-point (buf)
  (with-current-buffer buf (point)))

(defun test-line (buf)
  (with-current-buffer buf
    (line-number-at-pos)))

(describe "Core functions:"
	  :var (buf marker test-item)
	  (before-each
	   (with-current-buffer (setq buf (test-new-buffer))
	     (setq lister-local-left-margin 0
		   lister-local-top-margin 0
		   lister-local-bottom-margin 0)
	     (setq marker (make-marker))
	     (set-marker marker (point-max)))
	   (setq test-item '("1" "2")))
	  ;;
	  (it "Flatten an item list."
	      (expect (lister-strflat "test")         :to-equal '("test"))
	      (expect (lister-strflat '("1" nil "3")) :to-equal '("1" "3"))
	      (expect (lister-strflat '(("1") ("2"))) :to-equal '("1" "2")))
	  ;;
	  (it "Insert item using a marker position."
	      (lister-insert-lines buf marker test-item 0)
	      (expect (test-buffer-content buf)
		      :to-equal
		      "1\n2\n")
	      (expect (get-text-property marker 'item buf)
		      :to-be
		      t)
	      (expect (get-text-property marker 'nchars buf)
		      :to-be
		      4))
	  ;;
	  (it "Insert item using an integer position."
	      (lister-insert-lines buf (marker-position marker) test-item 0)
	      (expect (test-buffer-content buf)  :to-equal "1\n2\n"))
	  ;;
	  (it "Insert item with indentation level 1."
	      (lister-insert-lines buf (marker-position marker) test-item 1)
	      (expect (test-buffer-content buf)  :to-equal " 1\n 2\n"))
	  (it "Insert item with indentation level 2."
	      (lister-insert-lines buf (marker-position marker) test-item 2)
	      (expect (test-buffer-content buf)  :to-equal "  1\n  2\n"))
	  (it "Insert item with indentation level 3."
	      (lister-insert-lines buf (marker-position marker) test-item 3)
	      (expect (test-buffer-content buf)  :to-equal "   1\n   2\n")))

(describe "Inserting header, footer and items:"
	  :var (buf header footer data)
	  (before-each
	   (with-current-buffer 
	       (setq buf (lister-setup (test-new-buffer)
				       #'list))
	     (setq header "HEADER"
		   footer "FOOTER"
		   data   "DATA")
	     (setq lister-local-left-margin 0
		   lister-local-top-margin 0
		   lister-local-bottom-margin 0)))
	  ;;
	  (it "Fail calling 'insert' with a non-lister buffer."
	      (with-temp-buffer
		(expect  (lister-insert (current-buffer) (point-min) '("TEST" "TEST"))
			 :to-throw
			 'error)))
	  ;;
	  (it "Insert a single header."
	      (lister-set-header buf header)
	      (expect (test-buffer-content buf)
		      :to-equal  (concat header "\n")))
	  ;;
	  (it "Insert a single footer."
	      (lister-set-footer buf footer)
	      (expect (test-buffer-content buf)  :to-equal (concat footer "\n")))
	  ;;
	  (it "Insert header and footer."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat header "\n" footer "\n")))
	  ;;
	  (it "Insert header and footer with left margin."
	      (with-current-buffer buf
		(setq lister-local-left-margin 3))
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat (make-string 3 ? ) header "\n"
			      (make-string 3 ? ) footer "\n")))
	  (it "Insert header and footer with top margin."
	      (with-current-buffer buf
		(setq lister-local-top-margin 1))
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat "\n" header "\n" "\n" footer "\n")))
	  (it "Insert header and footer, then remove footer."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-set-footer buf nil)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat header "\n")))
	  (it "Insert header and footer, then remove header."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-set-header buf nil)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat footer "\n")))
	  (it "Add header and footer, then add a data item."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-add buf data)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat header "\n" data "\n" footer "\n")))
	  (it "Add header and footer, add data item, remove footer."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-add buf data)
	      (lister-set-footer buf nil)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat header "\n" data "\n")))
	  (it "Add header and footer, add data item, remove header."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-add buf data)
	      (lister-set-header buf nil)
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat data "\n" footer "\n")))
	  (it "Add header, footer, some list items."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-add buf "1")
	      (lister-add buf "2")
	      (lister-add buf "3")
	      (expect (test-buffer-content buf)
		      :to-equal
		      (concat header "\n1\n2\n3\n" footer "\n")))
	  (it "Add header, footer, some items, remove everything."
	      (lister-set-header buf header)
	      (lister-set-footer buf footer)
	      (lister-add buf "1")
	      (lister-add buf "2")
	      (lister-add buf "3")
	      (with-current-buffer buf
		(lister-goto buf :first)
		(lister-remove buf (point))
		(lister-remove buf (point))
		(lister-remove buf (point)))
	      (lister-set-header buf nil)
	      (lister-set-footer buf nil)
	      (expect (test-buffer-content buf)
		      :to-equal
		      ""))
	  (it "Replace items:"
	      (lister-add buf "1")
	      (lister-add buf "2")
	      (lister-add buf "nana")
	      (lister-replace buf :last "3")
	      (expect (lister-get-all-data buf)
		      :to-equal
		      '("1" "2" "3"))))

(describe "Hiding items:"
	  :var (buf first-item second-item
		    third-item fourth-item)
	  (before-each
	   (setq buf (lister-setup (test-new-buffer)
				   #'list
				   '("A" "B" "C" "D")))
	   (switch-to-buffer buf)
	   (setq first-item  (nth 0 lister-local-marker-list))
	   (setq second-item (nth 1 lister-local-marker-list))
	   (setq third-item  (nth 2 lister-local-marker-list))
	   (setq fourth-item (nth 3 lister-local-marker-list)))
	  (it "Hide item."
	      (lister-hide-item buf first-item)		      
	      (expect (invisible-p first-item)  :to-be   t)
	      (expect (invisible-p second-item) :to-be  nil))
	  (it "Hide and show it again."
	      (lister-hide-item buf first-item)
	      (lister-show-item buf first-item)
	      (expect (invisible-p first-item) :to-be  nil))
	  (it "Hide some items and then show all."
	      (lister-hide-item buf (seq-random-elt lister-local-marker-list))
	      (lister-hide-item buf (seq-random-elt lister-local-marker-list))
	      (lister-show-all-items buf)
	      (expect (invisible-p first-item) :to-be nil)
	      (expect (invisible-p second-item) :to-be nil)
	      (expect (invisible-p third-item) :to-be nil)
	      (expect (invisible-p fourth-item) :to-be nil)))

(describe "Moving to items:"
	  :var (buf header footer)
	  (before-each
	   (setq buf (lister-setup (test-new-buffer) #'list))
	   (setq header "HEADER"
		 footer "FOOTER")
	   (lister-add buf "1")
	   (lister-add buf "2")
	   (lister-add buf "3"))	  
	  (it "Move to the first item with :first."
	      (lister-goto buf :first)
	      (expect (test-line buf) :to-be 1))
	  (it "Move to the last item with :last."
	      (lister-goto buf :last)
	      (expect (test-line buf) :to-be 3))
	  (it "Add header, then move to first item."
	      (lister-set-header buf header)
	      (lister-goto buf :first)
	      (expect (test-line buf) :to-be 2))
	  (it "Add header, then move to the last item."
	      (lister-set-header buf header)
	      (lister-goto buf :last)
	      (expect (test-line buf) :to-be 4))
	  (it "Add header, remove random item, move to last item using :last."
	      (lister-set-header buf header)
	      (lister-remove buf
			     (with-current-buffer buf
			       (seq-random-elt lister-local-marker-list)))
	      (lister-goto buf :last)
	      (expect (test-line buf) :to-be 3)))

(describe "Using predicates:"
	  :var (buf datalist)
	  (before-each
	   (setq datalist '("AA" "AB" "BA" "BB"))
	   (setq buf (lister-setup (test-new-buffer)
				   #'list
				   datalist)))
	  (it "Generated copy of data is correct."
	      (expect (lister-get-visible-data buf)
		      :to-equal
		      datalist))
	  (it "Filter all data so that nothing is displayed."
	      (lister-add-filter buf (lambda (data) nil))
	      (lister-activate-filter buf)
	      (expect (lister-visible-markers buf)
		      :to-be
		      nil))
	  (it "Filter everything, then remove the filter."
	      (lister-add-filter buf (lambda (data) nil))
	      (lister-activate-filter buf)
	      (lister-clear-filter buf)
	      (lister-update-filter buf)
	      (expect (lister-get-visible-data buf)
		      :to-equal
		      datalist))
	  (it "Apply filter which matches only some data."
	      (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
	      (lister-activate-filter buf)
	      (expect (lister-get-visible-data buf)
		      :to-equal
		      '("AA" "AB")))
	  (it "Apply filter chain."
	      (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
	      (lister-add-filter buf (lambda (data) (string-match-p "B" data)))
	      (lister-activate-filter buf)
	      (expect (lister-get-visible-data buf)
		      :to-equal
		      '("AB")))
	  (it "Negate a filter chain."
	      (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
	      (lister-add-filter buf (lambda (data) (string-match-p "B" data)))
	      (lister-negate-filter buf)
	      (lister-activate-filter buf)
	      (expect (lister-get-visible-data buf)
		      :to-equal
		      '("AA" "BA" "BB"))))

(describe "Use hierarchies:"
	  :var (buf datalist)
	  (before-each
	   (setq buf (lister-setup (test-new-buffer)
				   #'list))
	   (setq datalist '("Item1" "Item2"
			    ("Subitem1" "Subitem2")
			    "Item3")))
	  ;;
	  (it "Add hierarchical list and get data tree:"
	      (lister-add-sequence buf datalist)
	      (expect (lister-get-all-data-tree buf)
		      :to-equal datalist))
	  (it "Insert items programmatically and get data tree:"
	      (lister-add buf "Item1")
	      (lister-add buf "Item2")
	      (lister-add buf "Subitem1" 1)
	      (lister-add buf "Subitem2" 1)
	      (lister-add buf "Item3" 0)
	      (expect (lister-get-all-data-tree buf)
		      :to-equal datalist))
	  (it "Fail to insert top item with level > 0:"
	      (lister-add buf "Item1" 1)
	      (with-current-buffer buf
		(lister-goto buf :first)
		(expect (lister-get-prop buf (point) 'level)
			:to-be
			0)))
	  (it "Delete a subtree:"
	      (lister-add-sequence buf datalist)
	      (with-current-buffer buf
		(lister-goto buf (elt lister-local-marker-list 2))
		(lister-remove-this-level buf (point)))
	      (expect (lister-get-all-data-tree buf)
		      :to-equal '("Item1" "Item2" "Item3"))))


(describe "Use a callback function:"
	  :var (value buf)
	  (before-each
	   (setq buf (lister-setup (test-new-buffer)
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
	      (expect value :to-equal "D"))
	  (it "Call the callback when leaving item."
	      (lister-add-leave-callback buf
					 (lambda ()
					   (setq value
						 (lister-get-data
						  buf
						  :point))))
	      (lister-goto buf :last)
	      (lister-goto buf :first)
	      (expect value :to-equal "D")))


(provide 'lister-tests)
;;; lister-tests.el ends here
