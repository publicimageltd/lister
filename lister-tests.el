;;; lister-tests.el --- testsuite for lister.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021

;; Author:  <joerg@joergvolbers.de>

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

;; Provides buttercup tests for lister

;;; Code:

(require 'lister "lister.el")
(require 'buttercup)
(require 'seq)
(require 'cl-lib)

(message "Testing lister version %s on Emacs %s" lister-version emacs-version)

;; * Utility functions for common setup tasks

(defun lister-test-new-buffer ()
  (generate-new-buffer "*LISTER*"))

(defun lister-test-buffer-content (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lister-test-buffer-content-with-properties (buf)
  (with-current-buffer buf (buffer-string)))

(defun lister-test-point (buf)
  (with-current-buffer buf (point)))

(defun lister-test-line (buf)
  (with-current-buffer buf
    (line-number-at-pos)))

(defun lister-test-setup-minimal-buffer ()
  "Set up a minimal buffer, with no margins and a list mapper.
Return the buffer object"
  (let ((new-buf (lister-setup (generate-new-buffer "*LISTER*")
			       #'list)))
    (with-current-buffer new-buf
      (setq lister-local-left-margin 0
	    lister-local-top-margin 0
	    lister-local-bottom-margin 0))
    new-buf))

(defun lister-test-positions-of (l &optional indentation)
  "Return a list of expected positions for inserting L.
L has to be a list of strings. The results are only valid in a
buffer with no margins, and if the items are inserted with no
indentation level.

Optional argument INDENTATION adds an indentation level of n."
  (let (acc (last-pos 1))
    (cl-dolist (item l)
      (push last-pos acc)
      (setq last-pos (+ last-pos (or indentation 0)
			(1+ (length item)))))
    (reverse acc)))

(defun lister-test-expected-content (l &optional header footer indentation)
  "Return a string of the expected buffer contents when inserting L.
L has to be a list of strings. HEADER and FOOTER have to be nil
or strings. The results are only valid in a minimal buffer with
no margins, and if the items are inserted with no indentation.

Optional argument INDENTATION adds an indentation level of n."
  (let ((indent-string (make-string (or indentation 0) ? )))
    (concat
     (when header (format "%s\n" header))
     (when l
       (concat (string-join (mapcar (apply-partially #'concat indent-string) l)
			    "\n")
	       "\n"))
     (when footer (format "%s\n" footer)))))

;; to match buffer contents:
(buttercup-define-matcher :to-have-as-content (buf content-to-be)
  (let* ((content (with-current-buffer (funcall buf)
		    (buffer-substring-no-properties
		     (point-min) (point-max))))
	 (expected-content (funcall content-to-be)))
    (buttercup--test-expectation
	(equal content expected-content)
      :expect-match-phrase
      (format "Expected buffer content to be '%s', but instead it was '%s'"
	      expected-content content)
      :expect-mismatch-phrase
      (format "Expected buffer content not to be '%s', but it was."
	      expected-content))))
  

;; -----------------------------------------------------------
;; The tests.

(describe "lister-insert-lines:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  ;; For constructing the expected content, each item's line is
  ;; treated as a separate item: The item ("1" "2") prints the same
  ;; result as a list of two single items ("1") and ("2").
  
  (it "Flatten an item list."    
    (expect (lister-strflat "test")         :to-equal '("test"))
    (expect (lister-strflat '("1" nil "3")) :to-equal '("1" "3"))
    (expect (lister-strflat '(("1") ("2"))) :to-equal '("1" "2")))
  
  (it "Insert item using a marker position."
    (let* ((marker     (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf marker test-item 0)
      (expect buf :to-have-as-content (lister-test-expected-content test-item))
      (expect (get-text-property marker 'item buf)   :to-be  t)
      (expect (get-text-property marker 'nchars buf) :to-be  4)))
  
  (it "Insert item using an integer position."
    (let* ((marker     (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 0)
      (expect buf :to-have-as-content (lister-test-expected-content test-item))))
  
  (it "Insert item with indentation level 1."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 1)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 1))))
  
  (it "Insert item with indentation level 2."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 2)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 2))))
  
  (it "Insert item with indentation level 3."
    (let* ((marker    (with-current-buffer buf (point-max-marker)))
	   (test-item '("1" "2")))
      (lister-insert-lines buf (marker-position marker) test-item 3)
      (expect buf :to-have-as-content (lister-test-expected-content test-item nil nil 3)))))

(describe "Some low level item functions:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))

  (describe "lister-insert:"
    (it "inserts item content as expected"
      (let ((item "JLKJLKDJFLKDSJFLKDJSLKFJSLKJFSLKJ"))
	(lister-insert buf (point-min) item)
	(expect buf :to-have-as-content (lister-test-expected-content (list item)))))
    (it "inserts item with properties nchars, item, level"
      (let* ((data "SOMEDATASHOULDBEGOOD")
	     (m (lister-insert buf (point-min) data)))
	(expect (get-text-property m 'nchars buf) :to-be (1+ (length data )))
	(expect (get-text-property m 'level buf)  :to-be 0)
	(expect (get-text-property m 'item buf)   :to-be t)))
    (it "inserts item with sensor function"
      (let* ((m (lister-insert buf (point-min) "A")))
	(expect (get-text-property m 'cursor-sensor-functions buf)
		:to-equal
		'(lister-sensor-function))))
    (it "inserts the new marker in the the marker list"
      (let* ((m (lister-insert buf (point-min) "A")))
	(expect (buffer-local-value 'lister-local-marker-list buf)
		:to-equal
		(list m)))))

  (describe "lister-next-free-position"
    (it "returns point-min in an empty buffer with no margins or heading"
      (expect (lister-next-free-position buf)
	      :to-be
	      (with-current-buffer buf (point-min))))
    (it "returns point-min in an empty buffer with footer"
      (lister-set-footer buf "A FOOTER")
      (expect (lister-next-free-position buf)
	      :to-be
	      (with-current-buffer buf (point-min))))
    (it "returns point after header if header is given"
      (let ((header "A HEADER"))
	(lister-set-header buf header)
	(expect (lister-next-free-position buf)
		:to-be
		;; add 1 nor item newline, 1 for next free pos:
		(+ 2 (length header)))))
    (it "returns point-max after last item if some items have been inserted"
      (let* ((some-items '("A" "B" "C" "D" "JA" "NEIN" "ACH" "EGAL")))
	(dolist (item some-items)
	  (lister-insert buf (point-max) item))
	(expect (lister-next-free-position buf)
		:to-be
		(with-current-buffer buf (point-max))))))

  ;; now we can use lister-add, which relies on lister-next-free-position:  
  (describe "lister-item-min"
    (it "returns the correct position if there is no item"
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 1 item"
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 2 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1))
    (it "returns the corrrect position after 3 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-min buf) :to-be 1)))
  
  (describe "lister-item-max"
    (it "returns the correct position if there is no item"
      (expect (lister-item-max buf) :to-be 1))
    (it "returns the corrrect position after 1 item"
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 3)) ;; 1+"A"+"\n"
    (it "returns the corrrect position after 2 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 5))
    (it "returns the corrrect position after 3 items"
      (lister-add buf "A")
      (lister-add buf "A")
      (lister-add buf "A")
      (expect (lister-item-max buf) :to-be 7)))    

  (describe "lister-looking-at-prop:"
    (it "looks at next item and returns nil if there is none"
      (expect (lister-looking-at-prop buf 1 'item 'next) :to-be nil))
    (it "looks at previous item and returns nil if there is none"
      (expect (lister-looking-at-prop buf 1 'item 'previous) :to-be nil))
    (it "looks at previous item and returns its position"
      (lister-add buf "A") ;; = pos 1
      (lister-add buf "A") ;; = pos 3
      (expect (lister-looking-at-prop buf 1 'item 'next)     :to-be 3))
    (it "looks at next item and returns its position"
      (lister-add buf "A") ;; = pos 1
      (lister-add buf "A") ;; = pos 3
      (expect (lister-looking-at-prop buf 3 'item 'previous) :to-be 1)))

  (describe "lister-rescan-item-markers:"
    (it "returns the correct positions for all items:"
      (let* ((some-items '("A" "B" "JAJA" "NEINEIN" "ACH")))
	(cl-dolist (item  some-items)
	  (lister-add buf item))
	(expect (mapcar #'marker-position (lister-rescan-item-markers buf))
		:to-equal
		(lister-test-positions-of some-items))))))

(describe "lister-marker-at:"
  :var (buf some-items some-positions)
  (before-each
    (setq some-items '("A" "B" "JAJA" "NEINEIN" "ACHJE"))
    (setq some-positions (lister-test-positions-of some-items))
    (setq buf (lister-test-setup-minimal-buffer))
    (cl-dolist (item some-items)
      (lister-add buf item)))
  (after-each
    (kill-buffer buf))
  
  (it "premise: inserted lists has the right positions"
    (expect (mapcar #'marker-position (lister-rescan-item-markers buf))
	    :to-equal
	    (lister-test-positions-of some-items)))
  (it "points correctly to the first item:"
    (expect (lister-get-data buf (lister-marker-at buf :first))
	    :to-equal (elt some-items 0)))
  (it "points correctly to the last item:"
    (expect (lister-get-data buf (lister-marker-at buf :last))
	    :to-equal
	    (car (reverse some-items))))
  (it "correctly points to some item at point:"
    (let (n)
      (with-current-buffer buf
	(setq n (random (seq-length some-items)))
	(goto-char (elt some-positions n)))
      (expect (lister-get-data buf :point)
	      :to-equal
	      (elt some-items n))))
  (it "accepts a marker as an argument"
    (let* ((position (elt some-positions 3))
	   (m1 (lister-make-marker buf position))
	   (m2 (lister-marker-at buf m1)))
      (expect m1 :to-equal m2)))
  (it "accepts an integer position as an argument"
    (let* ((position (elt some-positions 3))
	   (m (lister-marker-at buf position)))
      (expect (marker-position m) :to-equal position))))

(describe "Header and footer:"
  :var (buf header footer data)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq header "HEADER"
	  footer "FOOTER"
	  data   "DATA"))
  (after-each
    (kill-buffer buf))
  ;;
  (describe "Header and footer in empty lists:"
    
    (describe "lister-set-header"
      (it "inserts a single header."
	(lister-set-header buf header)
	(expect buf :to-have-as-content (lister-test-expected-content nil header)))
      (it "inserts a single header and removes it"
	(lister-set-header buf header)
	(lister-set-header buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil))))
    
    (describe "lister-set-footer"
      (it "inserts a single footer."
	(lister-set-footer buf footer)
	(expect buf :to-have-as-content (lister-test-expected-content nil nil footer)))
      (it "inserts a single footer and removes it"
	(lister-set-footer buf header)
	(lister-set-footer buf nil)
	(expect buf   :to-have-as-content (lister-test-expected-content nil))))
    
    (describe "set-header/set-footer combined: "
      (it "insert header and footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(expect buf :to-have-as-content (lister-test-expected-content nil header footer)))
      (it "insert header and footer, then remove footer"
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-set-footer buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil header)))
      (it "insert header and footer, then remove header."
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(lister-set-header buf nil)
	(expect buf :to-have-as-content (lister-test-expected-content nil nil footer))))
    
    (describe "set-header/-footer with margins:"
      (it "insert header and footer with left margin"
	(let ((margin 3))
	  (with-current-buffer buf
	    (setq lister-local-left-margin margin))
	  (lister-set-header buf header)
	  (lister-set-footer buf footer)
	  (let ((margined-header (concat (make-string 3 ? ) header))
		(margined-footer (concat (make-string 3 ? ) footer)))
	    (expect buf :to-have-as-content
		    (lister-test-expected-content nil margined-header margined-footer)))))
      (it "inserts header and footer with top margin."
	(with-current-buffer buf
	  (setq lister-local-top-margin 1))
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(expect buf :to-have-as-content (concat "\n" header "\n" "\n" footer "\n")))))

   (describe "Header and footer in non-empty lists"
     (it "adds one item between header and footer"
       (lister-set-header buf header)
       (lister-set-footer buf footer)
       (lister-add buf data)
       (expect buf :to-have-as-content (lister-test-expected-content (list data) header footer)))
     (it "adds some items between header and footer"
       (let ((some-items '("1" "2" "3")))
	 (lister-set-header buf header)
	 (lister-set-footer buf footer)
	 (cl-dolist (item some-items)
	   (lister-add buf item))
	 (expect buf :to-have-as-content (lister-test-expected-content some-items header footer))))
     (it "lister-set-footer nil removes footer"
       (lister-set-header buf header)
       (lister-set-footer buf footer)
       (lister-add buf data)
       (lister-set-footer buf nil)
       (expect buf :to-have-as-content (lister-test-expected-content (list data) header)))
     (it "lister-set-header nil removes header"
       (lister-set-header buf header)
       (lister-set-footer buf footer)
       (lister-add buf data)
       (lister-set-header buf nil)
       (expect buf :to-have-as-content (lister-test-expected-content (list data) nil footer)))
     (it "removes header and footer, leaving only the list:"
       (let ((some-items '("1" "2" "3")))
	 (lister-set-header buf header)
	 (lister-set-footer buf footer)
	 (cl-dolist (item some-items)
	   (lister-add buf item))
	 (lister-set-header buf nil)
	 (lister-set-footer buf nil)
	 (expect buf :to-have-as-content (lister-test-expected-content some-items))))))

(describe "Adding and replacing items"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("1" "2abc" "3")))
  (after-each
    (kill-buffer buf))

  (describe "lister-add"
    (it "returns correct marker positions:"
      (let* ((markers (mapcar (apply-partially #'lister-add buf)
			      some-items)))
	(expect (mapcar #'marker-position markers)
		:to-equal
		(lister-test-positions-of some-items))))
    (it "stores item positions in local marker list:"
      (let* ((markers (mapcar (apply-partially #'lister-add buf)
			      some-items)))
	(expect markers
		:to-equal
		(with-current-buffer buf
		  lister-local-marker-list)))))

  (describe "lister-replace"
    (it "replaces last item using position :last:"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (lister-replace buf :last "REPLACED")
      (expect buf :to-have-as-content
	      (lister-test-expected-content (append (butlast some-items 1)
						    '("REPLACED")))))
    (it "replaces all items while forwarding point"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (with-current-buffer buf
	(let ((new-items '("RAS" "DWA" "TRI")))
	  (goto-char (lister-item-min buf))	  
	  (cl-dolist (item new-items)
	    (lister-replace buf :point item)
	    (forward-line))
	  (expect buf :to-have-as-content (lister-test-expected-content new-items))))))

  (describe "lister-set-list"
    (it "replaces a whole list keeping header and footer"
      (let* ((header "HEADER")
	     (footer "FOOTER"))
	(cl-dolist (item some-items)
	  (lister-add buf item))
	(lister-set-header buf header)
	(lister-set-footer buf footer)
	(let ((new-data '("A" "B" "C")))
	  (lister-set-list buf new-data)
	  (expect buf :to-have-as-content (lister-test-expected-content new-data header footer)))))
    
    (it "replaces a whole list with no header or footer"
      (cl-dolist (item some-items)
	(lister-add buf item))
      (lister-set-header buf nil)
      (lister-set-footer buf nil)
      (let ((new-data '("A" "B" "C")))
	(lister-set-list buf new-data)
	(expect buf :to-have-as-content (lister-test-expected-content new-data))))))

(describe "lister-get-all-data"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "JHJH" "OUO" "KLL" "RZGVJ&&&%&%/")))
  (after-each
    (kill-buffer buf))
  
  (it "returns nil if list is empty"
    (expect (lister-get-all-data buf) :to-be nil))
  (it "returns all data as a flat list"
    (cl-dolist (item some-items)
      (lister-add buf item))
    (expect (lister-get-all-data buf) :to-equal some-items))
  (it "returns only data within a region"
    (cl-dolist (item some-items)
      (lister-add buf item))
    (let* ((positions (lister-test-positions-of some-items))
	   (beg-n     2)
	   (end-n     4))
      (expect (lister-get-all-data buf
				   (elt positions beg-n)
				   (elt positions end-n))
	      :to-equal
	      ;; positions always stand for the beginning of the item,
	      ;; so we have to expand the subsequence by 1
	      (seq-subseq some-items beg-n (1+ end-n)))))
  (it "returns all data even if it is marked as hidden"
    (cl-dolist (item some-items)
      (lister-hide-item buf (lister-add buf item)))
    (expect (lister-get-all-data buf) :to-equal some-items)))

(describe "linster-insert-sequence:"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))
  ;;
  (it "insert a list in the right order"
    (let ((the-sequence '("A" "B" "C" "D")))
      (lister-insert-sequence buf (lister-marker-at buf :first) the-sequence)
      (expect (lister-get-all-data buf) :to-equal the-sequence)))
  (it "inserts vector in the right order"
    (let ((the-sequence ["A" "B" "C" "D"]))
      (lister-insert-sequence buf (lister-marker-at buf :first) the-sequence)
      (expect (lister-get-all-data buf) :to-equal (seq-into the-sequence 'list)))))

(describe "lister-add-sequence"
  :var (buf)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer buf))
  
  (it "inserts two sequences one after another"
    (let ((the-sequence '("A" "B" "C" "D")))
      (lister-add-sequence buf the-sequence)
      (lister-add-sequence buf the-sequence)
      (expect (lister-get-all-data buf) :to-equal (append the-sequence the-sequence))))

  ;; this is just for time measurement:
  (xdescribe "takes its time when it"
    (it "adds a list with 1.000 items"
      (lister-add-sequence buf (make-list 1000 "Item")))
    (it "adds a list with 2.000 items:"
      (lister-add-sequence buf (make-list 2000 "I am an item")))
    (it "adds a with 5.000 items:"
      (lister-add-sequence buf (make-list 5000 "I am an item")))
    (it "adds a vector with 1.000 items:"
      (lister-add-sequence buf (make-vector 1000 "Item")))
    (it "adds a vector with 2.000 items:"
      (lister-add-sequence buf (make-vector 2000 "I am an item")))
    (it "adds a vector with 5.000 items:"
      (lister-add-sequence buf (make-vector 5000 "I am an item")))
    (it "adds and removes a list with 1.000 items:"
      (lister-add-sequence buf  (make-list 1000 "Item"))
      (lister-remove-this-level buf (lister-item-min buf)))
    (it "adds and removes a list with 2.000 items:"
      (lister-add-sequence buf (make-list 2000 "Item"))
      (lister-remove-this-level buf (lister-item-min buf)))
    (it "adds and removes a list with 5.000 items:"
      (lister-add-sequence buf (make-list 5000 "Item"))
      (lister-remove-this-level buf (lister-item-min buf)))))

;; REVIEW Rewrite with new matcher 
(describe "Hiding items:"
  :var (buf first-item second-item
	    third-item fourth-item)
  (before-each
    (setq buf (lister-setup (lister-test-new-buffer)
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

;; REVIEW Rewrite with new matcher
(describe "Moving to items:"
  :var (buf header)
  (before-each
    (setq buf (lister-setup (lister-test-new-buffer) #'list))
    (setq header "HEADER")
    (lister-add-sequence buf '("1" "2" "3"))
    (after-each
      (kill-buffer buf))
    ;;
    (it "Move to the first item with :first."
      (lister-goto buf :first)
      (expect (lister-test-line buf) :to-be 1))
    (it "Move to the last item with :last."
      (lister-goto buf :last)
      (expect (lister-test-line buf) :to-be 3))
    (it "Add header, then move to first item."
      (lister-set-header buf header)
      (lister-goto buf :first)
      (expect (lister-test-line buf) :to-be 2))
    (it "Add header, then move to the last item."
      (lister-set-header buf header)
      (lister-goto buf :last)
      (expect (lister-test-line buf) :to-be 4))
    (it "Add header, remove random item, move to last item using :last."
      (lister-set-header buf header)
      (lister-remove buf (with-current-buffer buf
			   (seq-random-elt lister-local-marker-list)))
      (lister-goto buf :last)
      (expect (lister-test-line buf) :to-be 3))))

;; REVIEW Rewrite with new matcher 
(describe "Indexed lists:"
  :var (buf)
  (before-each
    (setq buf (lister-setup (lister-test-new-buffer) #'list
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

;; REVIEW Rewrite with new matcher
(describe "Using predicates:"
  :var (buf datalist)
  (before-each
    (setq datalist '("AA" "AB" "BA" "BB"))
    (setq buf (lister-setup (lister-test-new-buffer)
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

;; REVIEW Rewrite with new matcher
(describe "Use hierarchies and indentation:"
  :var (buf datalist)
  (before-each
    (setq buf (lister-setup (lister-test-new-buffer)
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

;; REVIEW Maybe rewrite; check at least
(describe "Walk items:"
  :var (buf data)
  (before-each
    (setq buf (lister-setup (lister-test-new-buffer)
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
    (setq buf (lister-setup (lister-test-new-buffer)
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

;; REVIEW 
(describe "Mark and unmark items"
  :var (buf data)
  (before-each
    (setq data '("A" "B" "C" "D"))
    (setq buf (lister-setup (lister-test-new-buffer)
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
  (xit "Mark all items, return marked values"
    (lister-mark-all-items buf t)
    (expect  (lister-all-marked-items buf)
	     :to-equal (lister-get-all-data buf))))

(provide 'lister-tests)
;;; lister-tests.el ends here
