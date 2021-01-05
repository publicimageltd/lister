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
(require 'seq)
(require 'cl-lib)

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

;; -----------------------------------------------------------
;; The tests.

(describe "lister-insert-lines:"
  :var (buf marker test-item)
  (before-each
    (with-current-buffer (setq buf (test-new-buffer))
      (setq lister-local-left-margin 0
	    lister-local-top-margin 0
	    lister-local-bottom-margin 0)
      (setq marker (make-marker))
      (set-marker marker (point-max)))
    (setq test-item '("1" "2")))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Flatten an item list."
    (expect (lister-strflat "test")         :to-equal '("test"))
    (expect (lister-strflat '("1" nil "3")) :to-equal '("1" "3"))
    (expect (lister-strflat '(("1") ("2"))) :to-equal '("1" "2")))
  (it "Insert item using a marker position."
    (lister-insert-lines buf marker test-item 0)
    (expect (test-buffer-content buf)              :to-equal  "1\n2\n")
    (expect (get-text-property marker 'item buf)   :to-be  t)
    (expect (get-text-property marker 'nchars buf) :to-be  4))
  (it "Insert item using an integer position."
    (lister-insert-lines buf (marker-position marker) test-item 0)
    (expect (test-buffer-content buf)  :to-equal "1\n2\n"))
  (it "Insert item with indentation level 1."
    (lister-insert-lines buf (marker-position marker) test-item 1)
    (expect (test-buffer-content buf)  :to-equal " 1\n 2\n"))
  (it "Insert item with indentation level 2."
    (lister-insert-lines buf (marker-position marker) test-item 2)
    (expect (test-buffer-content buf)  :to-equal "  1\n  2\n"))
  (it "Insert item with indentation level 3."
    (lister-insert-lines buf (marker-position marker) test-item 3)
    (expect (test-buffer-content buf)  :to-equal "   1\n   2\n")))

(describe "Low-level item:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (test-new-buffer) #'list))
    (with-current-buffer buf
      (setq lister-local-left-margin 0
	    lister-local-top-margin 0
	    lister-local-bottom-margin 0)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "item-min/item-max returns correct position:"
    (expect (lister-item-min buf) :to-be 1)
    (expect (lister-item-max buf) :to-be 1)
    (lister-add buf "A")
    (expect (lister-item-min buf) :to-be 1)
    (expect (lister-item-max buf) :to-be 3)
    (lister-add buf "A")
    (expect (lister-item-max buf) :to-be 5))
  (it "looking-at-prop returns correct positions:"
    (lister-add buf "A")
    (lister-add buf "A")
    (expect (lister-looking-at-prop buf 3 'item 'previous) :to-be 1)
    (expect (lister-looking-at-prop buf 1 'item 'next)     :to-be 3)))
  
(describe "lister-marker-at:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (test-new-buffer)  #'list))
    (with-current-buffer buf
      (setq lister-local-left-margin 0
	    lister-local-top-margin 0
	    lister-local-bottom-margin 0))
      (lister-add buf "A") ;; integer position 1
      (lister-add buf "B") ;; 3
      (lister-add buf "C") ;; 5
      (lister-add buf "D") ;; 7
      (lister-add buf "E") ;; 9
      )
  (after-each
    (kill-buffer buf))
  ;;
  (it "Local marker list equals collected markers:"
    (let* ((llm (with-current-buffer buf lister-local-marker-list)))
      (expect llm :to-equal (lister-rescan-item-markers buf))))
  (it "Get the first item:"
    (let ((m (lister-marker-at buf :first)))
      (expect (lister-get-data buf m) :to-equal "A")))
  (it "Get the last item:"
    (let ((m (lister-marker-at buf :last)))
      (expect (lister-get-data buf m) :to-equal "E")))
  (it "Access value at point:"
    (lister-goto buf :first)
    (with-current-buffer buf  (forward-line))
    (let ((m (lister-marker-at buf :point)))
      (expect (lister-get-data buf m) :to-equal "B")))
  (it "Call it with a marker:"
    (let* ((m1 (lister-make-marker buf 9))
	   (m2 (lister-marker-at buf m1)))
      (expect m1 :to-equal m2)))
  (it "Call it with a position:"
    (let* ((p1 9)
	   (m1 (lister-marker-at buf p1)))
      (expect (marker-position m1) :to-equal p1))))

(describe "Set header, footer and items:"
  :var (buf header footer data)
  (before-each
    (with-current-buffer  (setq buf (lister-setup (test-new-buffer) #'list))
      (setq header "HEADER" footer "FOOTER" data   "DATA")
      (setq lister-local-left-margin 0
	    lister-local-top-margin 0
	    lister-local-bottom-margin 0)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Insert a single header."
    (lister-set-header buf header)
    (expect (test-buffer-content buf) :to-equal  (concat header "\n")))
  (it "Insert a single footer."
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf) :to-equal (concat footer "\n")))
  (it "Insert header and footer."
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (expect (test-buffer-content buf) :to-equal (concat header "\n" footer "\n")))
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
  (it "Check return values when adding items:"
    (let* ((m1 (lister-add buf "1"))
	   (m2 (lister-add buf "2"))
	   (m3 (lister-add buf "3")))
      (expect (list m1 m2 m3)
	      :to-equal
	      (with-current-buffer buf
		lister-local-marker-list))))
  (it "Replace last item:"
    (lister-add buf "1")
    (lister-add buf "2")
    (lister-add buf "nana")
    (lister-replace buf :last "3")
    (expect (lister-get-all-data buf)  :to-equal  '("1" "2" "3")))
  (it "Replace last and second item:"
    (let* ((m1 (lister-add buf "1"))
	   (m2 (lister-add buf "old-2"))
	   (m3 (lister-add buf "old-3"))
	   (lister-inhibit-cursor-action t))
      ;; Calling the two replacement actions the other way around
      ;; kills the marker m2, since inserting the new data also moves
      ;; the original marker one further down.
       (lister-replace buf m2 "2")
       (lister-replace buf m3 "3")
       (ignore m1)
      (expect (lister-get-all-data buf) :to-equal '("1" "2" "3"))))
  (it "Replace a whole list via set-list, with header and footer defined:"
    (cl-dolist (item '(1 2 3 4 5 6 7))
      (lister-add buf item))
    (lister-set-header buf header)
    (lister-set-footer buf footer)
    (let ((data '("A" "B" "C")))
      (lister-set-list buf data)
      (expect (lister-get-all-data buf)  :to-equal    data)
      (expect (test-buffer-content buf)  :to-equal   (format "%s\n%s\n%s\n" ;; footer and header each have a newline!
							     header
							     (string-join data "\n") ;; last item's newline
							     ;; is in the format spec
							     footer))))
  (it "Replace a whole list via set-list, with no header or footer:"
    (cl-dolist (item '(1 2 3 4 5 6 7))
      (lister-add buf item))
    (lister-set-header buf nil)
    (lister-set-footer buf nil)
    (let ((data '("A" "B" "C")))
      (lister-set-list buf data)
      (expect (lister-get-all-data buf) :to-equal   data)
      (expect (test-buffer-content buf) :to-equal  (concat (string-join data "\n") "\n")))))

(describe "Inserting sequences:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (test-new-buffer)  #'list)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Insert list in the right order:"
    (let ((the-sequence '("A" "B" "C" "D")))
      (lister-insert-sequence buf (lister-marker-at buf :first) the-sequence)
      (expect (lister-get-all-data buf) :to-equal the-sequence)))
  (it "Insert vector in the right order:"
    (let ((the-sequence ["A" "B" "C" "D"]))
      (lister-insert-sequence buf (lister-marker-at buf :first) the-sequence)
      (expect (lister-get-all-data buf) :to-equal (seq-into the-sequence 'list))))
  (it "Insert two sequences using lister-add-sequence:"
    (let ((the-sequence '("A" "B" "C" "D")))
      (lister-add-sequence buf the-sequence)
      (lister-add-sequence buf the-sequence)
      (expect (lister-get-all-data buf) :to-equal (append the-sequence the-sequence)))))

(describe "Inserting looong sequences:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (test-new-buffer)  #'list)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Add list with 1.000 items:"
    (lister-add-sequence buf (make-list 1000 "Item")))
  (it "Add list with 2.000 items:"
    (lister-add-sequence buf (make-list 2000 "I am an item")))
  (it "Add list with 5.000 items:"
    (lister-add-sequence buf (make-list 5000 "I am an item")))
  (it "Add vector with 1.000 items:"
    (lister-add-sequence buf (make-vector 1000 "Item")))
  (it "Add vector with 2.000 items:"
    (lister-add-sequence buf (make-vector 2000 "I am an item")))
  (it "Add vector with 5.000 items:"
    (lister-add-sequence buf (make-vector 5000 "I am an item")))
  (it "Add and remove list with 1.000 items:"
    (lister-add-sequence buf  (make-list 1000 "Item"))
    (lister-remove-this-level buf (lister-item-min buf)))
  (it "Add and remove list with 2.000 items:"
    (lister-add-sequence buf (make-list 2000 "Item"))
    (lister-remove-this-level buf (lister-item-min buf)))
  (it "Add and remove list with 5.000 items:"
    (lister-add-sequence buf (make-list 5000 "Item"))
    (lister-remove-this-level buf (lister-item-min buf))))

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
  (after-each
    (kill-buffer buf))
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
    (expect (invisible-p fourth-item) :to-be nil))
  (it "Return correct list of visible item markers."
    (lister-hide-item buf first-item)
    (expect (lister-get-visible-data buf)
	    :to-equal '("B" "C" "D"))))

(describe "Moving to items:"
  :var (buf header)
  (before-each
    (setq buf (lister-setup (test-new-buffer) #'list))
    (setq header "HEADER")
    (lister-add-sequence buf '("1" "2" "3"))
    (after-each
      (kill-buffer buf))
    ;;
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
      (lister-remove buf (with-current-buffer buf
			   (seq-random-elt lister-local-marker-list)))
      (lister-goto buf :last)
      (expect (test-line buf) :to-be 3))))

(describe "Indexed lists:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (test-new-buffer) #'list
			    '(0 1 2 3 4 5 6 7))))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Use index positions to access items:"
    (expect 
     (lister-get-data buf (lister-index-marker buf 0)) :to-equal  0)
    (expect 
     (lister-get-data buf (lister-index-marker buf 1)) :to-equal  1)
    (expect 
     (lister-get-data buf (lister-index-marker buf 3)) :to-equal  3)
    (expect 
     (lister-get-data buf (lister-index-marker buf 7)) :to-equal  7))
  (it "Return nil if index position is out of bounds:"
    (expect (lister-index-marker buf 2000)  :to-be  nil))
  (it "Find the index position of a marker:"
    (cl-loop for i from 0 to 7
	     do
	     (expect
	      (lister-index-position buf
				     (lister-index-marker buf i))
	      :to-be
	      i))))

(describe "Using predicates:"
  :var (buf datalist)
  (before-each
    (setq datalist '("AA" "AB" "BA" "BB"))
    (setq buf (lister-setup (test-new-buffer)
			    #'list
			    datalist)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Generated copy of data is correct."
    (expect (lister-get-visible-data buf) :to-equal   datalist))
  (it "Filter all data so that nothing is displayed."
    (lister-add-filter buf (lambda (data) (ignore data)))
    (lister-activate-filter buf)
    (expect (lister-visible-items buf)  :to-be   nil))
  (it "Filter everything, then remove the filter."
    (lister-add-filter buf (lambda (data) (ignore data)))
    (lister-activate-filter buf)
    (lister-clear-filter buf)
    (lister-update-filter buf)
    (expect (lister-get-visible-data buf) :to-equal datalist))
  (it "Apply filter which matches only some data."
    (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
    (lister-activate-filter buf)
    (expect (lister-get-visible-data buf) :to-equal  '("AA" "AB")))
  (it "Apply filter chain."
    (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
    (lister-add-filter buf (lambda (data) (string-match-p "B" data)))
    (lister-activate-filter buf)
    (expect (lister-get-visible-data buf) :to-equal  '("AB")))
  (it "Negate a filter chain."
    (lister-add-filter buf (lambda (data) (string-match-p "\\`A" data)))
    (lister-add-filter buf (lambda (data) (string-match-p "B" data)))
    (lister-negate-filter buf)
    (lister-activate-filter buf)
    (expect (lister-get-visible-data buf) :to-equal  '("AA" "BA" "BB"))))

(describe "Use hierarchies and indentation:"
  :var (buf datalist)
  (before-each
    (setq buf (lister-setup (test-new-buffer)
			    #'list))
    (setq datalist '("Item1" "Item2"
		     ("Subitem1" "Subitem2")
		     "Item3")))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Add hierarchical list and get data tree:"
    (lister-add-sequence buf datalist)
    (expect (lister-get-all-data-tree buf) :to-equal datalist))
  (it "Return the indentation level"
    (let* ((m1  (lister-add buf "Item1"))
	   (m2  (lister-add buf "Item2" 1))
	   (m3  (lister-add buf "Item3" 2)))
      (expect (lister-level-at buf m1) :to-be 0)
      (expect (lister-level-at buf m2) :to-be 1)
      (expect (lister-level-at buf m3) :to-be 2)))
  (it "Return data tree:"
    (lister-add buf "Item1")
    (lister-add buf "Item2")
    (lister-add buf "Subitem1" 1)
    (lister-add buf "Subitem2" 1)
    (lister-add buf "Item3" 0)
    (expect (lister-get-all-data-tree buf)  :to-equal datalist))
  (it "Fail to insert top item with level > 0:"
    (lister-add buf "Item1" 1)
    (with-current-buffer buf
      (lister-goto buf :first)
      (expect (lister-get-prop buf (point) 'level) :to-be   0)))
  (it "Delete a subtree:"
    (lister-add-sequence buf datalist)
    (with-current-buffer buf
      (lister-goto buf (elt lister-local-marker-list 2))
      (lister-remove-this-level buf (point)))
    (expect (lister-get-all-data-tree buf) :to-equal '("Item1" "Item2" "Item3")))
  (it "Delete a subtree below an item:"
    (lister-add-sequence buf datalist)
    (lister-remove-sublist-below buf (lister-index-marker buf 1))
    (expect (lister-get-all-data-tree buf)  :to-equal '("Item1" "Item2" "Item3"))))

(describe "Walk items:"
  :var (buf data)
  (before-each
    (setq buf (lister-setup (test-new-buffer)
			    (apply-partially #'format "%d")))
    (setq data  '(1 2 3 4 5 6 7 8 9 10))
    (lister-add-sequence buf data))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Walk all items:"
    (expect 
     (lister-walk-all buf #'identity)
     :to-be
     (length data))
    (let ((acc 0))
      (lister-walk-all buf (lambda (data)
			     (setq acc (+ acc data))))
      (expect
       acc
       :to-be
       (apply #'+ data))))
  ;;
  (it "Walk all items using predicate:"
    (expect 
     (lister-walk-all buf #'identity #'cl-evenp))
    :to-be
    (length (seq-filter #'cl-evenp data))))

(describe "Use a callback function:"
  :var (value buf callbackfn)
  (before-each
    (setq buf (lister-setup (test-new-buffer)
			    #'list
			    '("A" "B" "C" "D")))
    (switch-to-buffer buf) ;; we need the window to be set.
    (setq callbackfn (lambda ()  (setq value (lister-get-data
					      buf
					      :point))))
    (setq value nil))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Call the callback when entering item."
    (lister-add-enter-callback buf callbackfn)
    (lister-goto buf :last)
    (expect value :to-equal "D"))
  (it "Call the callback when leaving item."
    (lister-add-leave-callback buf callbackfn)
    (lister-goto buf :last)
    (lister-goto buf :first)
    (expect value :to-equal "D"))
  (it "Do not call callback within `lister-with-locked-cursor'"
    (lister-add-enter-callback buf callbackfn)
    (let (in-between-value)
      (lister-goto buf :first)
      (lister-with-locked-cursor buf
	(lister-goto buf :last)
	(setq in-between-value value)
	(lister-goto buf :first))
      (expect value :to-equal "A")
      (expect in-between-value :to-equal "A"))))

(describe "Mark and unmark items"
  :var (buf data)
  (before-each
    (setq data '("A" "B" "C" "D"))
    (setq buf (lister-setup (test-new-buffer)
			    #'list
			    data)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "Mark a single item and check text properties"
    (let* ((m (seq-random-elt
	       (with-current-buffer buf lister-local-marker-list))))
      (lister-mark-item buf m t)
      (expect (get-text-property m 'mark buf) :to-be  t)
      (expect (lister-get-mark-state buf m)   :to-be  t)))
  (it "Mark a single item, unmark it, check text properties"
    (let* ((m (seq-random-elt
	       (with-current-buffer buf lister-local-marker-list))))
      (lister-mark-item buf m t)
      (lister-mark-item buf m nil)
      (expect (get-text-property m 'mark buf) :to-be  nil)
      (expect (lister-get-mark-state buf m)   :to-be  nil)))
  (it "Mark all items, return marked values"
    (lister-mark-all-items buf t)
    (expect  (lister-all-marked-data buf)  :to-equal (lister-get-all-data buf))))

(provide 'lister-tests)
;;; lister-tests.el ends here
