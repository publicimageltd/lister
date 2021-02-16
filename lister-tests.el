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
(require 'cl-extra)

(message "Testing lister version %s on Emacs %s" lister-version emacs-version)

(setq buttercup-stack-frame-style 'pretty)

;; * Utility functions 

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

(defun lister-test-remove-elt-by-index (l n)
  "Remove element with index N (zero-based) from L."
  (append (cl-subseq l 0 n)
	  (cl-subseq l (1+ n))))

(defun lister-test-only-visible-content (buf)
  "Return only content from BUF which is not marked as invisible."
  (with-current-buffer buf
    (goto-char (point-min))
    (let (acc)
      (while (not (eobp))
	(let* ((invisible (get-text-property (point) 'invisible))
               (next-change
		(or (next-single-property-change (point) 'invisible)
                    (point-max))))
	  (unless invisible
	    (setq acc
		  (concat acc (buffer-substring-no-properties (point) next-change))))
          (goto-char next-change)))
      acc)))

;;; * Custom Matchers

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

;; to match only visible buffer contents:
(buttercup-define-matcher :to-have-as-visible-content (buf content-to-be)
  (let* ((content  (lister-test-only-visible-content (funcall buf)))
	 (expected-content (funcall content-to-be)))
    (buttercup--test-expectation
	(equal content expected-content)
      :expect-match-phrase
      (format "Expected buffer content to be '%s', but instead it was '%s'"
	      expected-content content)
      :expect-mismatch-phrase
      (format "Expected buffer content not to be '%s', but it was."
	      expected-content))))

;; to match the position of point:
(buttercup-define-matcher :to-have-point-value-of (buf pos-or-integer)
  (let* ((point (with-current-buffer (funcall buf) (point)))
	 (pos   (let ((poi (funcall pos-or-integer)))
		  (pcase poi
		    ((pred integerp) poi)
		    ((pred markerp) (marker-position poi))
		    (_              (error "invalid position: %s" poi))))))
    (buttercup--test-expectation
	(eq point pos)
      :expect-match-phrase
      (format "Expected point in buffer to be '%d', but instead it was '%d'."
	      pos point)
      :expect-mismatch-phrase
      (format "Expected point in buffer not to be '%d', but it was."
	      pos))))

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

;; * CORE: lister-insert, lister-next-free-position, lister-item-min,
;;         lister-item-max

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

;; * CORE: lister-marker-at

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

;; * Setting and removing header/footer

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

;; * Adding and replacing items

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

;; * Insert Sequence

(describe "lister-insert-sequence:"
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

;; * Showing / Hiding items

(describe "Hiding items:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "B" "C" "D" "E" "ETC" "PP"))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-hide-item"
    (it "hides an arbitrary item"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(with-current-buffer buf
	  (expect (invisible-p (elt positions n))  :to-be   t))))
    (it "does not hide the other items"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content 
		 (lister-test-remove-elt-by-index some-items n))))))

  (describe "lister-show-item"
    (it "unhides an item"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))
	(lister-hide-item buf (elt positions n))
	(lister-show-item buf (elt positions n))
	(expect buf :to-have-as-content
		(lister-test-expected-content some-items)))))

  (describe "lister-get-visible-data"
    (it "only returns visible item data"
      (let* ((positions (lister-test-positions-of some-items))
	     (n (random (length some-items))))      
	(lister-hide-item buf (elt positions n))
	(expect (lister-get-visible-data buf)
		:to-equal
		(lister-test-remove-elt-by-index some-items n))))))

;; * Movement / Navigation

(describe "Navigation:"
  :var (buf some-items positions)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("1" "2" "3" "4" "5" "6" "7"))
    (setq positions (lister-test-positions-of some-items))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-goto "
    (it "jumps to the first item when called with :first"
      (lister-goto buf :first)
      (expect buf :to-have-point-value-of (elt positions 0)))
    (it "jumps to the last item when called with :last"
      (lister-goto buf :last)
      (expect buf :to-have-point-value-of (elt positions (1- (length some-items)))))
    (it "accepts an integer position as argument"
      (let* ((n (random (length some-items)))
	    (int-pos (elt positions n)))
	(lister-goto buf int-pos)
	(expect buf :to-have-point-value-of (elt positions n))))
    (it "accepts :point as argument"
      (let* ((n (random (length some-items)))
	    (int-pos (elt positions n)))
	(with-current-buffer buf
	  (goto-char int-pos))
	(lister-goto buf :point)
	(expect buf :to-have-point-value-of (elt positions n))))
    (it "throws an error if going to an invisible item"
      (let ((pos (elt positions 2)))
	(lister-hide-item buf pos)
	(expect (lister-goto buf pos) :to-throw))))

  (describe "lister-with-locked-cursor: "
    (it "moves cursor if body has shrunk the list"
      (let ((expected-last-item  (elt positions (- (length some-items) 2))))
	(lister-goto buf :last)
	(expect (with-current-buffer buf (point))
		:not :to-be expected-last-item)
	(lister-with-locked-cursor buf
	  (lister-remove buf :last))
	(expect (with-current-buffer buf (point))
		:to-be expected-last-item)))))

;; * Index

(describe "Indexes:"
  :var (buf some-items positions)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "K" "8989" "dskjfs" "(NKJKHKJ" ")" "HA!"))
    (setq positions (lister-test-positions-of some-items))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-index-position: "
    (it "returns the correct index number when called with integer positions"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (expect (lister-index-position buf (elt positions n))
		       :to-be
		       n)))
    (it "returns the correct index number when called with marker positions"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (let ((m (with-current-buffer buf
			  (goto-char (elt positions n))
			  (point-marker))))
		 (expect (lister-index-position buf m)
			 :to-be
			 n))))
    (it "returns nil if position is invalid"
      (expect (lister-index-position buf 23000) :to-be nil)))

  (describe "lister-index-marker: "
    (it "returns the correct marker for a given index position"
      (cl-loop for n from 0 below (length some-items)
	       do
	       (let ((m (lister-index-marker buf n)))
		 (expect (marker-position m) :to-be (elt positions n)))))
    (it "returns nil if index position is out of bound"
      (let ((n-max (+ (length some-items) 20)))
	(expect (lister-index-marker buf n-max) :to-be nil)))))

;; * Filter

(describe "Using filter:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    ;; don't change the items, filters rely on the content
    (setq some-items '("AAAAA" "ABBBBBB"
		       "BAAAA" "BBBBBB"
		       "SOMETHING"
		       "CAAAA" "CBBBBB"))
    (lister-add-sequence buf some-items))
  (after-each
    (kill-buffer buf))

  (describe "lister-set-filter"
    (it "does nothing if it is called with nil in a new buffer"
      (spy-on 'lister-filter-all-items)
      (lister-set-filter buf nil)
      (expect 'lister-filter-all-items :not :to-have-been-called))
    (it "hides all items matching the passed filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data))))
	(lister-set-filter buf filter-fn)
	(expect buf
		:to-have-as-visible-content
		(lister-test-expected-content
		 (seq-filter filter-fn some-items)))))
    (it "clears the filter when called with nil in a buffer with active filter"
      (let ((filter-fn (lambda (data)
			 (string-match-p "\\`A" data))))
	(lister-set-filter buf filter-fn)
	(lister-set-filter buf nil)
	(expect buf :to-have-as-visible-content
		(lister-test-expected-content some-items)))))
  
  (describe "lister-with-locked-cursor "
    ;; -set-filter calls -walk, which in turn is wrapping its body in
    ;; lister-with-locked-cursor. So just calling set-filter is enough
    ;; to test that macro.
    (it "keeps the visual line if the item is filtered away in its body"
      ;; we move point to the first item, filter the first two items
      ;; of the list, and then expect point to be on the third item,
      ;; which is visually the first visible item.
      (let* ((filter-fn (lambda (data) ;; do not display the first two
			  (not
			   (string-match-p "\\`A" data))))
	     (expected-pos (elt (lister-test-positions-of some-items) 2)))
	(lister-goto buf :first) ;; this first item will be  matched by filter-fn
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf filter-fn)
	(expect buf :to-have-point-value-of expected-pos)))
    (it "jumps to the last visible line if the current line has been filtered away in the body"
      ;; we move to the last line, filter the last element, and then
      ;; expect cursor to have moved
      (let* ((filter-fn (lambda (data) ;; do not display the last two
			  (not
			   (string-match-p "\\`C" data))))
	     (filtered-items (seq-filter filter-fn some-items))
	     (expected-pos (elt (lister-test-positions-of some-items)
				(- (length some-items)
				   ;; subtract the number of removed items
				   (- (length some-items) (length filtered-items))
				   ;; and subtract one since the index
				   ;; is zero-based
				   1))))
	(lister-goto buf :last)
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf filter-fn)
	(expect buf :to-have-point-value-of expected-pos)))
    (it "jumps to the first line if the body filters all items"
      (let ((expected-pos (elt (lister-test-positions-of some-items) 0)))
	(lister-goto buf :last)
	(expect buf :not :to-have-point-value-of expected-pos)
	(lister-set-filter buf (lambda (_) nil))
	(expect buf :to-have-point-value-of expected-pos)))
    (it "stays at the first item if everything is shown again"
      (let ((expected-pos (elt (lister-test-positions-of some-items) 0)))
	(lister-goto buf :first)
	(lister-set-filter buf (lambda (_) nil))
	(lister-set-filter buf (lambda (_) t))
	(expect buf :to-have-point-value-of expected-pos)))))


;; * Levels and indentation

(describe "Levels and indentation:"
  :var (buf some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    ;; don't change the list, it is used in the specs below
    (setq some-items '("Item1" "Item2"
		       ("Subitem1" "Subitem2")
		       "Item3"
		       ("Another Subitem1" "Another Subitem2"))))
  (after-each
    (kill-buffer buf))
  ;;
  (describe "lister-add-sequence "
    (it "assigns nested lists a higher level"
      (lister-add-sequence buf some-items)
      (let (acc)
	(lister-walk-all buf (lambda (_) (push (lister-level-at buf (point)) acc)))
	(setq acc (reverse acc))
	(expect acc :to-equal '(0 0 1 1 0 1 1)))))
  (describe "lister-get-all-data-tree "
    (it "returns the nested list"
      (lister-add-sequence buf some-items)
      (expect (lister-get-all-data-tree buf)) :to-equal some-items))
  (describe "lister-add"
    (it "sets level as told by optional argument"
      (lister-add buf "Item1")
      (lister-add buf "Subitem1" 1)
      (lister-add buf "Item2" 0)
      (expect (lister-get-all-data-tree buf)
	      :to-equal '("Item1" ("Subitem1") "Item2")))
    (it "silently corrects level of top item if > 0:"
      (lister-add buf "Item1" 1)
      (expect (lister-level-at buf :first) :to-be 0))
    (it "silently corrects level of inserted item so that it only indents one more"
      (lister-add buf "Item1" 0)
      (let ((m (lister-add buf "Item2" 3)))
	(expect (lister-level-at buf m) :to-be 1)))
    (it "does not change the level of the new item if it only indents one more"
      (lister-add buf "Item1" 0)
      (let ((m (lister-add buf "Item2" 1)))
	(expect (lister-level-at buf m) :to-be 1))))

  (describe "lister-remove-this-level"
    (it "removes all subsequent items sharing the same level"
      (lister-add-sequence buf some-items)
      ;; remove from within the first nested element = #3 in the list
      (lister-remove-this-level buf (lister-index-marker buf 2))
      (expect (lister-get-all-data-tree buf)
	      :to-equal
	      (lister-test-remove-elt-by-index some-items 2))))

  (describe "lister-remove-sublist-below"
    (it "removes all indented items below an item"
      (lister-add-sequence buf some-items)
      ;; remove below the last 0-level item, which is #2 in the list 
      (lister-remove-sublist-below buf (lister-index-marker buf 1))
      (expect (lister-get-all-data-tree buf)
	      :to-equal
	      (lister-test-remove-elt-by-index some-items 2)))))

;; * Walk items

(describe "Walk items:"
  :var (buf data walk-positions walk-path)
  (before-each
    (setq buf  (lister-setup (generate-new-buffer "*LISTER*") 
			     (apply-partially #'format "%d")))
    ;; item at index 0 has value 0, at 1 has 1, etc.
    (setq data (number-sequence 0 10))
    ;; define a subset of items
    (setq walk-path '(3 6 8 9))
    ;; find the positions of this subset
    (lister-add-sequence buf data)
    (setq walk-positions (mapcar (apply-partially 'lister-index-marker buf) walk-path))
  (after-each
    (kill-buffer buf))

  (describe "lister-walk-some"
    (it "walks those items passed as marker list"
      (expect (lister-walk-some buf walk-positions #'identity)
	      :to-equal walk-path))
    (it "silently skips positions out of bound"
      (expect (lister-walk-some buf
				(append walk-positions '(23000 26000 234000))
				#'identity)
	      :to-equal walk-path))
    (it "silently skips positions which are not on the cursor gap"
      (expect (lister-walk-some buf
				(append walk-positions (mapcar #'1+ walk-positions))
				#'identity)
	      :to-equal walk-path))
    (it "skips positions where predicate is not matching"
      (expect (lister-walk-some buf walk-positions #'identity  #'cl-evenp)
	      :to-equal (seq-filter #'cl-evenp walk-path))))))
      

(describe "Use a callback function:"
  :var (value buf callbackfn some-items)
  (before-each
    (setq buf (lister-test-setup-minimal-buffer))
    (setq some-items '("A" "B" "C" "D"))
    (lister-add-sequence buf some-items)
    (switch-to-buffer buf) ;; we need the window to be set.
    (setq callbackfn (lambda ()
		       (setq value (lister-get-data
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
    (setq buf (lister-test-setup-minimal-buffer))
    (setq data '("A" "B" "C" "D"))
    (lister-add-sequence buf data))
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
