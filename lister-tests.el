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

;; Buttercup tests for lister

;;; Code:

(require 'lister "lister.el")
(require 'lister-mode "lister-mode.el")
(require 'buttercup)
(require 'seq)
(require 'cl-lib)
(require 'cl-extra)
(require 'rx)

;; TODO Implement custom-matcher "to-return-no-node", checking for nil
;; and giving more useful feedback if a node has been returned


;; (message "Testing lister version %s on Emacs %s" lister-version emacs-version)

;; (setq buttercup-stack-frame-style 'pretty)

(defvar lister-local-left-margin)

(defun lister-test-setup-minimal-buffer ()
  "Set up a minimal buffer, with no margins and a simple list mapper.
Return the ewoc object"
  (let ((ewoc (lister-setup "*LISTER*" #'list)))
    (with-current-buffer (ewoc-buffer ewoc)
      (setq-local lister-local-left-margin 0))
    ewoc))

(defun lister-test-get-data-at-point (ewoc)
  "Return the data of the item at point in EWOC.
This is functionally equivalent to `lister-get-data-at', but uses
low-lewel ewoc functions instead of `lister--parse-position'."
  (lister-node-get-data (with-current-buffer (ewoc-buffer ewoc)
                          (ewoc-locate ewoc))))

(defun lister-test-node-data-pred (value)
  "Build function matching a lister node's data having VALUE."
  (lambda (n) (equal (lister-node-get-data n)
                     value)))

(defun lister-test-data-pred (value)
  "Build function matching item having VALUE."
  (lambda (item) (equal value item)))

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
      (or acc ""))))

;;; * Custom Matchers

;; to match nodes by data:
(buttercup-define-matcher :to-be-node-with-data (node1 node2)
  (let* ((n1      (funcall node1))
         (n1-data (lister-node-get-data n1))
         (n2-data (funcall node2)))
    (buttercup--test-expectation
        (equal n1-data n2-data)
      :expect-match-phrase
      (format "Expected node data to be %s, but instead it was %s"
              n2-data (if n1 n1-data "no node at all"))
      :expect-mismatch-phrase
      (format "Expected node data not to be '%s', but instead it was"
              n2-data))))

;; to match nodes:
(buttercup-define-matcher :to-be-node (node1 node2)
  (let* ((n1   (funcall node1))
         (n2   (funcall node2))
         (n1-data (when n1 (lister-node-get-data n1)))
         (n2-data (when n2 (lister-node-get-data n2))))
    (buttercup--test-expectation
        (equal n1 n2)
      :expect-match-phrase
      (format "Expected node to be %s, but instead it was %s"
              n2-data (if n1 n1-data "no node at all"))
      :expect-mismatch-phrase
      (format "Expected node not to be '%s', but instead it was"
              n2-data))))

(defun lister-test--map-nodes (n)
  (mapcar #'lister-node-get-data n))

(buttercup-define-matcher :to-be-nodes (a b)
  (cl-destructuring-bind
      ((a-expr . a) (b-expr . b))
      (mapcar #'buttercup--expr-and-value (list a b))
    (let* ((a-uniques (cl-set-difference a b :test #'equal))
           (b-uniques (cl-set-difference b a :test #'equal))
           (spec (format-spec-make
                  ?A (format "%S" a-expr)
                  ?a (format "%S" (lister-test--map-nodes a))
                  ?B (format "%S" b-expr)
                  ?b (format "%S" (lister-test--map-nodes b))
                  ?m (format "%S" (lister-test--map-nodes b-uniques))
                  ?p (format "%S" (lister-test--map-nodes a-uniques)))))
      (cond
       ((and a-uniques b-uniques)
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same nodes as `%b', but `%m' are missing and `%p' are present unexpectedly in `%a'."
                   spec)))
       (a-uniques
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same nodes as `%b', but `%p' are present unexpectedly in `%a'."
                   spec)))
       (b-uniques
        (cons nil (buttercup-format-spec
                   "Expected `%A' to contain the same nodes as `%b', but `%m' are missing in `%a'."
                   spec)))
       (t
        (cons t (buttercup-format-spec
                 "Expected `%A' not to have same nodes as `%b'"
                 spec)))))))

;; to match position of point in list:
(buttercup-define-matcher :to-have-point-at-item (ewoc data)
  (let* ((ewoc-value (funcall ewoc))
         (data-value (funcall data))
         (ewoc-data  (lister-test-get-data-at-point ewoc-value)))
    (buttercup--test-expectation
        (equal ewoc-data data-value)
      :expect-match-phrase (format "Expected point to be at item `%s', but it was at `%s'"
                                   data-value ewoc-data)
      :expect-mismatch-phrase (format "Expected point not to be at item `%s', but it was."
                                      ewoc-data))))


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

;; -----------------------------------------------------------
;; * Tests for lister.el

;;; * Set up
(describe "Set up:"
  (describe "lister-setup"
    (it "returns an ewoc object"
      (let* ((ewoc (lister-setup "TEST" #'list)))
        (expect (ewoc-p ewoc)
                :to-be-truthy)))
    (it "creates a new buffer by name"
      (let* ((ewoc (lister-setup "TEST" #'list)))
        (expect (buffer-name (ewoc-buffer ewoc))
                :to-equal
                "TEST")))
    (it "does not change the major mode of an existing buffer"
      (let* ((buf (generate-new-buffer "TEST"))
             (mode (with-current-buffer buf major-mode))
             (ewoc (lister-setup "TEST" #'list)))
        (expect (with-current-buffer (ewoc-buffer ewoc) major-mode)
                :to-be
                mode)))
    (it "stores the mapper locally in the buffer"
      (let* ((ewoc (lister-setup "TEST" 'list)))
        (expect (buffer-local-value 'lister-local-mapper
                                    (ewoc-buffer ewoc))
                :to-equal
                'list)))))

(describe "Basic level list handling:"
  :var (ewoc l item)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D" "E" "F"))
    (setq item "ITEM"))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-empty-p:"
    (it "returns t when list is empty:"
      (expect (lister-empty-p ewoc)
              :to-be-truthy))
    (it "returns nil if list has an item:"
      (ewoc-enter-first ewoc (lister--item-create :data "TEST"))
      (expect (lister-empty-p ewoc)
              :not :to-be-truthy)))

  (describe "lister-set-list/lister-get-list:"
    (it "setting and getting returns an unchanged flat list:"
      (lister-set-list ewoc l)
      (expect (lister-get-list ewoc) :to-equal  l))

    (it "setting and getting returns an unchanged nested list:"
      (let ((the-list (list "TOP" l "SECOND" l "THIRD" '("1" "2" ("3")) "FOURTH" "FIFTH")))
        (lister-set-list ewoc the-list)
        (expect (lister-get-list ewoc)
                :to-equal the-list))))

  (describe "lister--determine-level"
    (it "always returns 0 if called with with prev-level nil:"
      (expect (lister--determine-level nil 12)
              :to-equal 0)
      (expect (lister--determine-level nil 0)
              :to-equal 0)
      (expect (lister--determine-level nil 123214)
              :to-equal 0))
    (it "always returns prev-level+1 if called with a bigger value:"
      (expect (lister--determine-level 0 12)
              :to-be 1)
      (expect (lister--determine-level 0 1)
              :to-be 1)
      (expect (lister--determine-level 0 2)
              :to-be 1))
    (it "it always returns the new level if called with smaller value:"
      (expect (lister--determine-level 2 1)
              :to-be 1)
      (expect (lister--determine-level 2 0)
              :to-be 0)))

  (describe "lister-set-list:"
    (it "nil clears the current list:"
      (lister-set-list ewoc l)
      (lister-set-list ewoc nil)
      (expect (lister-empty-p ewoc)
              :to-be-truthy)
      (expect (lister-get-list ewoc)
              :to-be nil)))

  (describe "lister-get-list:"
    (it "returns a slice when called with integer arguments:"
      (lister-set-list ewoc l)
      (expect (lister-get-list ewoc 0 1)
              :to-equal '("A" "B"))
      (expect (lister-get-list ewoc 1 2)
              :to-equal '("B" "C"))
      (expect (lister-get-list ewoc 4 5)
              :to-equal '("E" "F")))
    (it "returns the tail of the list:"
      (lister-set-list ewoc l)
      (expect (lister-get-list ewoc 3)
              :to-equal '("D" "E" "F")))
    (it "returns the head of the list:"
      (lister-set-list ewoc l)
      (expect (lister-get-list ewoc nil 3)
              :to-equal '("A" "B" "C" "D")))
    (it "combines node and integer positions as arguments:"
      (lister-set-list ewoc l)
      (let ((beg (ewoc-nth ewoc 0))
            (end 3))
        (expect (lister-get-list ewoc beg end)
                :to-equal '("A" "B" "C" "D")))
      (let ((beg 0)
            (end (ewoc-nth ewoc 3)))
        (expect (lister-get-list ewoc beg end)
                :to-equal '("A" "B" "C" "D")))))

  (describe "lister-map"
    (it "maps a flat list"
      (lister-set-list ewoc l)
      (cl-labels ((map-fn (s)
                          (concat "MAPPED" s)))
        (expect (lister-map ewoc #'map-fn)
                :to-equal (mapcar #'map-fn l))))
    (it "maps a flat list with predicate"
      (lister-set-list ewoc l)
      (cl-labels ((map-fn (s)
                          (concat "MAPPED" s))
                  (pred-fn (s)
                           (string= "A" s)))
        (expect (lister-map ewoc #'map-fn #'pred-fn)
                :to-equal '("MAPPEDA"))))

    (it "maps a hierarchical list"
      (let ((l '("A" "B" "C" ("D" "E" "F"))))
        (lister-set-list ewoc l)
        (cl-labels ((map-fn (s)
                            (concat "MAPPED" s)))
          (expect (lister-map ewoc #'map-fn)
                  :to-equal '("MAPPEDA" "MAPPEDB" "MAPPEDC"
                              ("MAPPEDD" "MAPPEDE" "MAPPEDF")))))))

  (describe "lister-add"
    (it "adds a single item to an empty list:"
      (lister-add ewoc item)
      (expect (lister-get-list ewoc)
              :to-equal (list item)))
    (it "always adds item at the end of the list:"
      (dolist (item l)
        (lister-add ewoc item))
      (expect (lister-get-list ewoc)
              :to-equal l)))

  (describe "lister-add-list:"
    (it "adds a flat list to an empty list:"
      (lister-add-list ewoc l)
      (expect (lister-get-list ewoc)
              :to-equal l))
    (it "adds a flat-list to a non-empty list:"
      (lister-add ewoc item)
      (lister-add-list ewoc l)
      (expect (lister-get-list ewoc)
              :to-equal (append (list item) l)))
    (it "adds a nested list to an empty list:"
      (let ((nested '("1" "2" ("3" "4") "5" "6")))
        (lister-add-list ewoc nested)
        (expect (lister-get-list ewoc)
                :to-equal nested))))

  (describe "lister-insert:"
    (it "adds a single item in an empty list:"
      (lister-insert ewoc :first item)
      (expect (lister-get-list ewoc)
              :to-equal (list item)))
    (it "inserts an item at the top of an existing list:"
      (lister-set-list ewoc l)
      (lister-insert ewoc :first item)
      (expect (lister-get-list ewoc)
              :to-equal (append (list item) l)))
    (it "inserts an item at the end of an existing list:"
      (lister-set-list ewoc l)
      (lister-insert ewoc :last item nil t)
      (expect (lister-get-list ewoc)
              :to-equal (append l (list item))))
    (it "throws an error if position does not exist:"
      (lister-set-list ewoc l)
      (expect (lister-insert ewoc 10 item)
              :to-throw))
    (it "inserts an item before the last item:"
      (lister-set-list ewoc l)
      (lister-insert ewoc :last item)))

  (describe "lister-replace-at"
    (it "replaces an item in an existing list:"
      (lister-set-list ewoc l)
      (lister-replace-at  ewoc :first item)
      (expect (lister-get-list ewoc)
              :to-equal (append (list item)
                                (cdr l)))))
  (describe "lister-delete-at:"
    (it "deletes the first item of a list:"
      (lister-set-list ewoc l)
      (lister-delete-at ewoc :first)
      (expect (lister-get-list ewoc)
              :to-equal (cdr l)))
    (it "deletes the last item of a list:"
      (lister-set-list ewoc l)
      (lister-delete-at ewoc :last)
      (expect (lister-get-list ewoc)
              :to-equal (reverse (cdr (reverse l)))))
    (it "deletes the only item of a list:"
      (lister-set-list ewoc (list item))
      (lister-delete-at ewoc :first)
      (expect (lister-empty-p ewoc)
              :to-be-truthy))
    (it "throws an error if node does not exist:"
      (expect (lister-delete-at ewoc :first)
              :to-throw))
    (it "throws an error if index is out of range:"
      (lister-set-list ewoc l)
      (expect (lister-delete-at ewoc 10)
              :to-throw)))

  (describe "lister-replace-list"
    (it "replaces a list from node to node:"
      (lister-set-list ewoc l)
      (let ((first  (ewoc-nth ewoc 1)) ;; "B"
            (second (ewoc-nth ewoc 4)) ;; "E"
            (new    '("1" "2" "3" "4" "5" "6" "7")))
        (lister-replace-list ewoc new first second)
        (expect (lister-get-list ewoc)
                :to-equal (append (list "A") new (list "F")))))
    (it "replaces a list with index positions:"
      (lister-set-list ewoc l)
      (let* ((new      '("1" "2" "3" "4" "5" "6" "7"))
             (expected (append (list "A") new (list "F"))))
        (lister-replace-list ewoc new 1 4)
        (expect (lister-get-list ewoc)
                :to-equal expected)))
    (it "completely replaces a list with :first :last"
      (lister-set-list ewoc l)
      (let ((new '("1" "2")))
        (lister-replace-list ewoc new :first :last)
        (expect (lister-get-list ewoc) :to-equal new)))
    (it "completely replaces a list with nil nil:"
      (lister-set-list ewoc l)
      (let ((new '("1" "2")))
        (lister-replace-list ewoc new nil nil)
        (expect (lister-get-list ewoc) :to-equal new)))
    (it "replaces the list from ... up to end:"
      (lister-set-list ewoc l)
      (let ((new '("1" "2")))
        (lister-replace-list ewoc new 2 nil)
        (expect (lister-get-list ewoc)
                :to-equal '("A" "B" "1" "2"))))
    (it "replaces the existing list with an empty list:"
      (lister-set-list ewoc l)
      (lister-replace-list ewoc nil nil nil)
      (expect (lister-empty-p ewoc) :to-be-truthy))
    (it "replaces an empty list with a new list:"
      (lister-replace-list ewoc '("1" "2") nil nil)
      (expect (lister-get-list ewoc) :to-equal '("1" "2"))))


  ;; TODO Add tests for -update; -delete-marked
  (describe "modified flag:"
    (describe "lister-setup"
      (it "returns buffer with modified set to nil"
        (expect (lister-modified-p ewoc) :not :to-be-truthy)))
    (describe "lister-insert"
      (it "sets modified flag"
        (lister-insert ewoc :first "TEST")
        (expect (lister-modified-p ewoc) :to-be-truthy)))
    (describe "lister-replace-at"
      (it "sets modified flag"
        (lister-insert ewoc :first "TEST")
        (lister-set-modified-p ewoc nil)
        (expect (lister-modified-p ewoc) :not :to-be-truthy)
        (lister-replace-at ewoc :first "NEW ITEM")
        (expect (lister-modified-p ewoc) :to-be-truthy)))
    (describe "lister-delete-at"
      (it "sets modified flag"
        (lister-insert ewoc :first "TEST")
        (lister-set-modified-p ewoc nil)
        (expect (lister-modified-p ewoc) :not :to-be-truthy)
        (lister-delete-at ewoc :first)
        (expect (lister-modified-p ewoc) :to-be-truthy)))
    (describe "lister-delete-list"
      (it "sets modified flag"
        (lister-insert-list ewoc :first '("A" "B" "C"))
        (lister-set-modified-p ewoc nil)
        (expect (lister-modified-p ewoc) :not :to-be-truthy)
        (lister-delete-list ewoc :first :last)
        (expect (lister-modified-p ewoc) :to-be-truthy)))
    (describe "lister-delete-all"
      (it "sets modified flag"
        (lister-insert-list ewoc :first '("A" "B" "C"))
        (lister-set-modified-p ewoc nil)
        (expect (lister-modified-p ewoc) :not :to-be-truthy)
        (lister-delete-all ewoc)
        (expect (lister-modified-p ewoc) :to-be-truthy)))))

;;; * Setting and removing header/footer

(describe "Header/Footer:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("1" "2" "3")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-set-header:"
    (it "sets header in an empty list:"
      (lister-set-header ewoc "TEST")
      (expect (ewoc-buffer ewoc)
              :to-have-as-content "TEST\n"))
    (it "removes header when called with nil:"
      (lister-set-header ewoc "TEST")
      (lister-set-header ewoc nil)
      (expect (ewoc-buffer ewoc)
              :to-have-as-content ""))
    (it "accepts a list of strings:"
      (lister-set-header ewoc '("TEST" "TEST"))
      (expect (ewoc-buffer ewoc)
              :to-have-as-content "TEST\nTEST\n"))
    (it "adds header before a list:"
      (lister-set-list ewoc l)
      (lister-set-header ewoc "TEST")
      (expect (ewoc-buffer ewoc)
              :to-have-as-content
              (concat "TEST\n"
                      (string-join l "\n")
                      "\n")))))

;;; * Diverse API

(describe "Diverse API"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D" "E" "F")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-with-boundaries"
    (xit "throws an error if not called with symbols"
      ;; This test does not fail, but causes annoying "eager macro
      ;; expansion" error; so we x it
      (expect (lister-with-boundaries ewoc 1 0
                (ignore))
              :to-throw)
      (expect (lister-with-boundaries ewoc :first :last
                (ignore))
              :to-throw))
    (it "passes through nodes"
      (lister-set-list ewoc l)
      (let ((first (ewoc-nth ewoc 0))
            (second (ewoc-nth ewoc 1)))
        (expect (lister-with-boundaries ewoc first second
                  first)
                :to-be-node first)))
      (it "accepts :first, :last as values"
        (lister-set-list ewoc l)
        (let ((first :first)
              (second :last))
          (expect (lister-with-boundaries ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-boundaries ewoc first second
                    second)
                  :to-be-node (ewoc-nth ewoc -1))))
      (it "accepts indices as values"
        (lister-set-list ewoc l)
        (let ((first 0)
              (second (1- (length l))))
          (expect (lister-with-boundaries ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-boundaries ewoc first second
                    second)
                  :to-be-node (ewoc-nth ewoc -1))))
      (it "accepts nil as values"
        (lister-set-list ewoc l)
        (let (first second)
          (expect (lister-with-boundaries ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-boundaries ewoc first second
                    second)
                  :to-be-node (ewoc-nth ewoc -1))))))

;;; * Looping

(describe "Loops:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D" "E" "F")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-dolist-nodes"
    (it "loops over the complete list of nodes with explicit boundaries:"
      (lister-set-list ewoc l)
      (let (acc)
        (lister-dolist-nodes (ewoc node :first :last)
          (push (lister--item-data (ewoc-data node)) acc))
        (expect acc :to-equal (reverse l))))
    (it "loops over the complete list of nodes with implicit boundaries:"
      (lister-set-list ewoc l)
      (let (acc)
        (lister-dolist-nodes (ewoc node)
          (push (lister--item-data (ewoc-data node)) acc))
        (expect acc :to-equal (reverse l))))
    (it "can deal with BODY which deletes the node:"
      (lister-set-list ewoc l)
      (lister-dolist-nodes (ewoc node)
        (let ((inhibit-read-only t))
          (ewoc-delete ewoc node)))
      (expect (lister-empty-p ewoc) :to-be-truthy))
    (it "breaks out of the loop with a cl-return:"
      (lister-set-list ewoc l)
      (let ((target-node (ewoc-nth ewoc 2))
            acc)
        (lister-dolist-nodes (ewoc node)
          (push (lister--item-data (ewoc-data node)) acc)
          (when (eq target-node node)
            (cl-return)))
        (expect (nreverse acc) :to-equal '("A" "B" "C")))))

  (describe "lister-dolist:"
    (it "loops over the data items:"
      (lister-set-list ewoc l)
      (let (acc)
        (lister-dolist (ewoc data :first :last)
          (push data acc))
        (expect acc :to-equal (reverse l))))
    (it "gives access to the data's node:"
      (lister-set-list ewoc l)
      (let (acc)
        (lister-dolist (ewoc data :first :last node)
          (push (lister--item-data (ewoc-data node)) acc))
        (expect acc :to-equal (reverse l)))))

  (describe "lister-collect-list"
    (it "collects a flat list:"
      (lister-set-list ewoc l)
      (expect (lister-collect-list ewoc)
              :to-equal l))
    (it "transforms the collected data:"
      (lister-set-list ewoc l)
      (let ((transformer-fn (apply-partially #'concat " ")))
        (expect (lister-collect-list ewoc :first :last
                                nil
                                transformer-fn)
                :to-equal
                (mapcar transformer-fn l))))
    (it "only returns matching data items:"
      (lister-set-list ewoc l)
      (let ((predicate-fn (apply-partially #'string-match-p (rx string-start (or "A" "B" "C")))))
        (expect (lister-collect-list ewoc :first :last
                                  predicate-fn)
                :to-equal '("A" "B" "C"))))
    (it "combines pred and transformer functions:"
      (lister-set-list ewoc l)
      (let ((predicate-fn (apply-partially #'string-match-p (rx string-start (or "A" "B" "C"))))
            (transformer-fn (apply-partially #'concat " ")))
        (expect (lister-collect-list ewoc :first :last
                                  predicate-fn
                                  transformer-fn)
                :to-equal (mapcar transformer-fn '("A" "B" "C"))))))

  (describe "lister-update-list"
    (it "updates the whole list:"
      (lister-set-list ewoc l)
      (let ((action-fn (apply-partially #'concat "X")))
        (lister-update-list ewoc action-fn)
        (expect (lister-get-list ewoc)
                :to-equal (mapcar action-fn l))))
    (it "updates only those items matching pred-fn:"
      (lister-set-list ewoc l)
      (let ((action-fn (apply-partially #'concat "X"))
            (pred-fn   (apply-partially #'string-match "B")))
        (lister-update-list ewoc action-fn)
        (lister-update-list ewoc action-fn :first :last pred-fn)
        (expect (lister-get-list ewoc)
                :to-equal (mapcar (lambda (s)
                                    (if (funcall pred-fn s)
                                        (funcall action-fn s)
                                      s))
                                  (mapcar action-fn l)))))))

;;; * Sublists

(describe "Sublists:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D"))
    (setq sub-l '("1" "2" "3")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "insert-sublist-below:"
    (it "inserts sublist below position"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc :first sub-l)
      (expect (lister-get-list ewoc)
              :to-equal (list "A"
                              sub-l
                               "B" "C" "D")))
    (it "inserts sublist as normal list on top:"
      (lister-insert-sublist-below ewoc :first sub-l)
      (expect (lister-get-list ewoc)
              :to-equal sub-l))
    (it "produces a nested list if called within a sublist:"
      (lister-set-list ewoc sub-l)
      (lister-insert-sublist-below ewoc 0 sub-l)
      (lister-insert-sublist-below ewoc 1 sub-l)
      (lister-insert-sublist-below ewoc 2 sub-l)
      (expect (lister-get-list ewoc)
              :to-equal
              '("1"
                ("1"
                 ("1"
                  ("1" "2" "3")
                  "2"
                  "3")
                 "2"
                 "3")
                "2"
                "3"))))

  (describe "lister--locate-sublist:"
    :var (ewoc l)
    (before-each
      (setq ewoc (lister-test-setup-minimal-buffer))
      (setq l '("1" "2" "3" "4")))
    (after-each
      (kill-buffer (ewoc-buffer ewoc)))

    (it "returns the whole list boundaries if there is no sublist"
      (lister-set-list ewoc '("0" "1" "2" "3" "4" "5" "6"))
      (let ((expected-bounds (list (lister--parse-position ewoc :first)
                                   (lister--parse-position ewoc :last))))
        (expect (lister--locate-sublist ewoc 2)
                :to-be-nodes expected-bounds)))
    (it "returns nil if there are no list items"
      (lister-delete-all ewoc)
      (expect (lister--locate-sublist ewoc :first)
              :to-be nil))
    (it "throws an error if POS is out of bounds"
      (lister-set-list ewoc '("0" "1"))
      (expect (lister--locate-sublist ewoc 2)
              :to-throw))
    (it "identifies a surrounded sublist:"
      (lister-set-list ewoc '("0" "1" ("2" "3" "4" "5") "4" "5"))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 5))))
        (expect (lister--locate-sublist ewoc 2)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 4)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 5)
                :to-be-nodes expected-bounds)))
    (it "identifies a sublist with 'open end':"
      (lister-set-list ewoc '("0" "1"
                           ("2" "3" "4" "5")))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 5))))
        (expect (lister--locate-sublist ewoc 2)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 4)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 5)
                :to-be-nodes expected-bounds)))
    (it "identifies a surrounded nested sublist:"
      (lister-set-list ewoc '("0" ("1" ("2" "3") "4" "5")))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 3))))
        (expect (lister--locate-sublist ewoc 2)
                :to-be-nodes expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-be-nodes expected-bounds)))
    (it "skips filtered items if asked for"
      (lister-set-list ewoc '("0" ("A" "A" "3" "4") "5"))
      (lister-set-filter ewoc (lambda (s) (equal s "A")))
      (let ((expected-bounds (list (ewoc-nth ewoc 3)
                                   (ewoc-nth ewoc 4))))
        (expect (lister--locate-sublist ewoc 2 :only-visible)
                :to-be-nodes expected-bounds)))
    (it "returns the only visible item if the rest of the sublist is filtered"
      (lister-set-list ewoc '("0" ("A" "A" "3" "A") "5"))
      (lister-set-filter ewoc (lambda (s) (equal s "A")))
      (let ((expected-bounds (list (ewoc-nth ewoc 3)
                                   (ewoc-nth ewoc 3))))
      (expect (lister--locate-sublist ewoc 2 :only-visible)
              :to-be-nodes expected-bounds)))
    (it "returns nil if the complete list is invisible"
      (lister-set-list ewoc '("A" ("A" "A" "A" "A") "A"))
      (lister-set-filter ewoc (lambda (s) (equal s "A")))
      (expect (lister--locate-sublist ewoc 2 :only-visible)
              :to-be nil))
    (it "returns first and last if all sublist items are filtered"
      (lister-set-list ewoc '("0" ("A" "A" "A" "A") "5"))
      (lister-set-filter ewoc (lambda (s) (equal s "A")))
      (let ((expected-bounds (list (ewoc-nth ewoc 0)
                                   (ewoc-nth ewoc 5))))
      (expect (lister--locate-sublist ewoc 2 :only-visible)
              :to-be-nodes expected-bounds))))

  (describe "lister-get-sublist-at:"
    (it "returns a sublist:"
      (lister-set-list ewoc '("0" ("1" "2" "3") "4"))
      (expect (lister-get-sublist-at ewoc 1)
              :to-equal '("1" "2" "3")))
    (it "returns the whole list if called with :first"
      (lister-set-list ewoc '("0" ("1" "2" "3") "4"))
      (expect (lister-get-sublist-at ewoc :first)
              :to-equal '("0" ("1" "2" "3") "4"))))

  (describe "lister-get-sublist-below:"
    (it "returns a sublist:"
      (lister-set-list ewoc '("0" ("1" "2" "3") "4"))
      (expect (lister-get-sublist-below ewoc 0)
              :to-equal '("1" "2" "3")))
    (it "returns nil if there is no sublist below:"
      (lister-set-list ewoc '("0" "1" "2" "3" "4"))
      (expect (lister-get-sublist-below ewoc 0)
              :to-be nil)))

  (describe "lister-delete-sublist-at"
    (it "deletes a sublist:"
      (lister-set-list ewoc '("0" ("1" "2" "3") "4"))
      (lister-delete-sublist-at ewoc 1)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "4")))
    (it "deletes the whole list if called with a level 0 item:"
      (lister-set-list ewoc '("0" ("1" ("2" "3")) "4" "5"))
      (lister-delete-sublist-at ewoc 0)
      (expect (lister-empty-p ewoc)
              :to-be-truthy)
      (expect (lister-get-list ewoc)
              :to-be nil)))

  (describe "lister-delete-sublist-below"
    (it "deletes a sublist below POS:"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc 0 sub-l)
      (expect (lister-get-list ewoc)
              :to-equal '("A" ("1" "2" "3") "B" "C" "D"))
      (lister-delete-sublist-below ewoc 0)
      (expect (lister-get-list ewoc) :to-equal l))
    (it "does nothing if there is no sublist at POS:"
      (lister-set-list ewoc l)
      (lister-delete-sublist-below ewoc 0)
      (expect (lister-get-list ewoc) :to-equal l)))
  (describe "lister-replace-sublist-below"
    (it "replaces a sublist below POS:"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc 0 sub-l)
      (lister-replace-sublist-below ewoc 0 '("A"))
      (expect (lister-get-list ewoc)
              :to-equal '("A" ("A") "B" "C" "D")))
    (it "deletes sublist if called with empty new list:"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc 0 sub-l)
      (lister-replace-sublist-below ewoc 0 nil)
      (expect (lister-get-list ewoc) :to-equal l)))
  (describe "lister-replace-sublist-at"))


;;; * Movement / Navigation

(describe "Movement ('Finding') and Navigation:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("0" "1" "2" "3" "4" "5"))
    (lister-set-list ewoc l))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister--next-node-matching"
    (it "returns the node matching PRED-FN looking down"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc 0)
                                          (lister-test-node-data-pred "4")
                                          #'ewoc-next)
              :to-be-node (lister-get-node-at ewoc 4)))
    (it "returns the node matching PRED-FN looking up"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc -1)
                                          (lister-test-node-data-pred "1")
                                          #'ewoc-prev)
              :to-be-node (lister-get-node-at ewoc 1)))
    (it "returns nil if no matching node is found looking down"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc 0)
                                          #'ignore
                                          #'ewoc-next)
              :to-be nil))
    (it "returns nil if no matching node is found looking up"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc -1)
                                          #'ignore
                                          #'ewoc-prev)
              :to-be nil))
    (it "ignores the starting node matching PRED-FN"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc 0)
                                          (lister-test-node-data-pred "0")
                                          #'ewoc-next)
              :to-be nil))
    (it "stops searching when node LIMIT is reached"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc 0)
                                          (lister-test-node-data-pred "3")
                                          #'ewoc-next
                                          (lister-get-node-at ewoc 2))
              :to-be nil))
    (it "cancels searching even if LIMIT matches PRED-FN"
      (expect (lister--next-node-matching ewoc (ewoc-nth ewoc 0)
                                          (lister-test-node-data-pred "3")
                                          #'ewoc-next
                                          (lister-get-node-at ewoc 3))
              :to-be nil)))

  (describe "lister--next-or-this-node-matching"
    (it "returns the node matching PRED-FN looking down"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc 0)
                                                  (lister-test-node-data-pred "3")
                                                  #'ewoc-next)
              :to-be-node (lister-get-node-at ewoc 3)))
    (it "returns the node matching PRED-FN looking up"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc -1)
                                                  (lister-test-node-data-pred "2")
                                                  #'ewoc-prev)
              :to-be-node (lister-get-node-at ewoc 2)))
    (it "returns the starting node matching PRED-FN looking down"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc 0)
                                                  (lister-test-node-data-pred "0")
                                                  #'ewoc-next)
              :to-be-node (lister-get-node-at ewoc 0)))
    (it "returns the starting node matching PRED-FN looking up"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc -1)
                                                  (lister-test-node-data-pred "5")
                                                  #'ewoc-prev)))
    (it "stops searching before reaching LIMIT"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc 0)
                                                  (lister-test-node-data-pred "4")
                                                  #'ewoc-next
                                                  (lister-get-node-at ewoc 3))
              :to-be nil))
    (it "stops searching at LIMIT matching PRED-FN"
      (expect (lister--next-or-this-node-matching ewoc (ewoc-nth ewoc 0)
                                                  (lister-test-node-data-pred "3")
                                                  #'ewoc-next
                                                  (lister-get-node-at ewoc 3))
              :to-be nil)))

  (describe "lister-goto:"
    (it "moves point to the first item of the list:"
      (lister-goto ewoc :first)
      (expect ewoc :to-have-point-at-item "0"))
    (it "moves point to the last item of the list:"
      (lister-goto ewoc :last)
      (expect ewoc :to-have-point-at-item "5"))
    (it "moves point to the next item:"
      (lister-goto ewoc :first)
      (lister-goto ewoc :next)
      (expect ewoc :to-have-point-at-item "1"))
    (it "moves point to the prev item:"
      (lister-goto ewoc :last)
      (lister-goto ewoc :prev)
      (expect ewoc :to-have-point-at-item "4"))
    (it "moves point to an index position:"
      (lister-goto ewoc 4)
      (expect ewoc :to-have-point-at-item "4"))
    (it "throws an error if index is out of bounds:"
      (expect (lister-goto ewoc 8)
              :to-throw)))

  (describe "lister-top-sublist-node"
    (it "finds the first sublist node"
      (lister-set-list ewoc '("A" "B" ("AA" "BB" "CC" "DD") "C" ))
      (expect (lister-top-sublist-node ewoc 5)
              :to-be-node-with-data "AA"))
    (it "moves to the top node if there is no sublist"
      (expect (lister-top-sublist-node ewoc 4)
              :to-be-node-with-data "0"))
    (it "does nothing if there is no list"
      (lister-set-list ewoc nil)
      (expect (lister-top-sublist-node ewoc :point) :to-be nil)))

  (describe "lister-bottom-sublist-node"
    (it "moves to the last sublist node"
      (lister-set-list ewoc '("A" "B" ("AA" "BB" "CC" "DD") "C"))
      (expect (lister-bottom-sublist-node ewoc 5) :to-be-node-with-data "DD"))
    (it "does nothing if there is no list"
      (lister-set-list ewoc nil)
      (expect (lister-bottom-sublist-node ewoc :point) :to-be nil))
    (it "moves to the last node if there is no sublist"
      (expect (lister-bottom-sublist-node ewoc 3) :to-be-node-with-data "5")))

  (describe "lister-parent-node"
    (before-each
      ;;                       0   1    2    3   4     5       6     7
      (lister-set-list ewoc '("A" "B" ("AA" "BB" "CC" ("AAA" "BBB") "DD") "C" "D"))
      (lister-set-filter ewoc nil))
    (it "finds the parent node"
      (expect (lister-parent-node ewoc 4) :to-be-node-with-data "B"))
    (it "finds the parent node in a nested list"
      (expect (lister-parent-node ewoc 6) :to-be-node-with-data "CC"))
    (it "moves to the top node when called on a 0 level node"
      (expect (lister-parent-node ewoc 1) :to-be-node-with-data "A"))
    (it "moves to the top node when called from below a sublist"
      (expect (lister-parent-node ewoc 8) :to-be-node-with-data "A"))
    (it "skips real parent node since it is invisible"
      (lister-set-filter ewoc (lambda (s) (equal s "B")))
      (expect (lister-parent-node ewoc 3) :to-be-node-with-data "A"))
    (it "throws an error if all parent nodes are invisible"
      (lister-set-filter ewoc (lambda (s) (or (equal s "A")
                                              (equal s "B"))))
      (expect (lister-parent-node ewoc 2) :to-be nil))
    (it "moves to the first sublist node if all parent nodes are invisible"
      (lister-set-filter ewoc (lambda (s) (or (equal s "A")
                                              (equal s "B"))))
      (expect (lister-parent-node ewoc 3) :to-be-node-with-data "AA")))

  (describe "lister-first-sublist-node"
    (before-each
      ;;                       0   1    2    3   4     5       6     7
      (lister-set-list ewoc '("A" "B" ("AA" "BB" "CC" ("AAA" "BBB") "DD") "C" "D")))
    (it "returns nilif no sublist item is available"
      (expect (lister-first-sublist-node ewoc 5 'down) :to-be nil)
      (expect (lister-first-sublist-node ewoc 5 'up)   :to-be nil))
    (it "moves to first sublist item"
      (expect (lister-first-sublist-node ewoc :first 'down) :to-be-node-with-data "AA")
      (expect (lister-first-sublist-node ewoc :last 'up)  :to-be-node-with-data "DD"))
    (it "moves to nested sublist item"
      (expect (lister-first-sublist-node ewoc (lister-first-sublist-node ewoc :first 'down) 'down) :to-be-node-with-data "AAA")
      (expect (lister-first-sublist-node ewoc (lister-first-sublist-node ewoc :last 'up) 'up) :to-be-node-with-data "BBB"))))

;;; * Filter

(describe "Filter:"
  :var (ewoc l pred-fn
             filter-a filter-non-a
             filter-b
             filter-i filter-non-i
             filter-2nd
             filter-*)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l    '("AZ" "BAZ" "DAZ" "MAZ"
                 ("ITEM4" "ITEM5" "ITEM6")
                 "HEY" "THEY" "OBEY"))
    (lister-set-list ewoc l)
    (setq filter-a     (apply-partially #'string-match "A")
          filter-2nd   (apply-partially #'string-match-p "BAZ")
          filter-non-a (lambda (s) (not (string-match "A" s)))
          filter-b     (apply-partially #'string-match "Y")
          filter-i     (apply-partially #'string-match "ITEM")
          filter-non-i (lambda (s) (not (funcall filter-i s)))
          filter-*     (apply-partially #'string-match ".*")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "Finding nodes while filter is OFF:"
    (describe "lister--next-visible-node:"
      (it "moves to the next node:"
        (expect (lister--next-visible-node ewoc (ewoc-nth ewoc 0))
                :to-be-node (ewoc-nth ewoc 1)))
      (it "moves to prev node:"
          (expect (lister--next-visible-node ewoc (ewoc-nth ewoc -1) #'ewoc-prev)
                  :to-be-node (ewoc-nth ewoc -2)))
      (it "returns nil if already at last node:"
        (expect (lister--next-visible-node ewoc (ewoc-nth ewoc -1))
                :to-be nil))
      (it "returns nil if already at first node:"
        (expect (lister--next-visible-node ewoc (ewoc-nth ewoc 0) #'ewoc-prev)
                :to-be nil)))
    (describe "lister--first-visible-node:"
      (it "returns first node:"
        (expect (lister--first-visible-node ewoc)
                :to-be-node (ewoc-nth ewoc 0))))
    (describe "lister--last-visible-node:"
      (it "returns last node:"
        (expect (lister--last-visible-node ewoc)
                :to-be-node (ewoc-nth ewoc -1))))
    (describe "lister-next-matching"
      (it "finds next node matching a data predicate:"
        (expect (lister-next-matching ewoc :first
                                      (lister-test-data-pred "ITEM4"))
                :to-be-node (ewoc-nth ewoc 4)))
      (it "stops when reaching LIMIT"
        (expect (lister-next-matching ewoc :first
                                      (lister-test-data-pred "ITEM5")
                                      3)
                :to-be nil))
      (it "ignores LIMIT even if it matches PRED-FN"
        (expect (lister-next-matching ewoc :first
                                      (lister-test-data-pred "ITEM5")
                                      5)
                :to-be nil)))
    (describe "lister-prev-matching"
      (it "finds prev node matching a data predicate:"
        (expect (lister-prev-matching ewoc :last
                                      (lister-test-data-pred "ITEM4"))
                :to-be-node (ewoc-nth ewoc 4))))
    (describe "Hide visible items:"
      (describe "lister-set-filter:"
        (it "hides all items:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-*)
          (expect (ewoc-buffer ewoc)
                  :to-have-as-visible-content "")))))

    (describe "Finding nodes while filter is ON:"
      (describe "lister--first-visible-node:"
        (it "finds the first visible node with filter-a:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-a)
          (expect (lister--first-visible-node ewoc)
                  :to-be-node (ewoc-nth ewoc 4)))
        (it "finds the first visible node with filter-b:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-b)
          (expect (lister--first-visible-node ewoc)
                  :to-be-node (ewoc-nth ewoc 0)))
        (it "returns nil if everything is hidden:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-*)
          (expect (lister--first-visible-node ewoc)
                  :to-be nil)))
      (describe "lister--last-visible-node:"
        (it "finds the last visible node with filter-a:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-a)
          (expect (lister--last-visible-node ewoc)
                  :to-be-node (ewoc-nth ewoc -1)))
        (it "finds the last visible node with filter-b:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-b)
          (expect (lister--last-visible-node ewoc)
                  :to-be-node (ewoc-nth ewoc 6)))
        (it "returns nil if everyhing is hidden:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-*)
          (expect (lister--last-visible-node ewoc)
                  :to-be nil)))
      (describe "lister-next-visible-matching:"
        (it "skips hidden nodes when searching for an item:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-2nd)
          (let ((pred (lambda (s) (string-match-p "AZ" s))))
            (expect (lister-next-visible-matching ewoc :first pred)
                    :to-be-node (ewoc-nth ewoc 2))))
        (it "returns nil when searching for an hidden item:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-i)
          (let ((pred (lambda (s) (string-match-p "ITEM4" s))))
            (expect (lister-next-visible-matching ewoc :first pred)
                    :to-be nil))))
      (describe "lister-prev-visible-matching"
        (it "skips hidden nodes when searching for an item:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-2nd)
          (let ((pred (lambda (s) (string-match-p "AZ" s))))
            (expect (lister-prev-visible-matching ewoc 2 pred)
                    :to-be-node (ewoc-nth ewoc 0))))
        (it "returns nil when searching for a hiden item:"
          (lister-set-list ewoc l)
          (lister-set-filter ewoc filter-i)
          (let ((pred (lambda (s) (string-match-p "ITEM4" s))))
            (expect (lister-prev-visible-matching ewoc :last pred)
                    :to-be nil)))))
  (describe "retreiving lists with filter ON:"
    (describe "lister-get-list:"
      (it "returns complete list if everything is hidden:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-*)
        (expect (lister-get-list ewoc)
                :to-equal l)))
    (describe"lister-get-visible-list:"
      (it "returns empty list if everything is hidden:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-*)
        (expect (lister-get-visible-list ewoc)
                :to-be nil))
      (it "returns a flat filtered list:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-non-a)
        (expect (lister-get-visible-list ewoc)
                :to-equal '("AZ" "BAZ" "DAZ" "MAZ")))
      (it "returns a sublist:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-non-i)
        (expect (lister-get-visible-list ewoc)
                ;; NOTE Indented list!
                :to-equal '(("ITEM4" "ITEM5" "ITEM6"))))))

  (describe "filter new items:"
    (describe "lister-insert:"
      (it "inserts and hide items which match current filter:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-*)
        (lister-add ewoc "NEU")
        (expect (let ((lister-ignore-filter t))
                  (lister-get-list ewoc))
                :to-equal (append l (list "NEU")))
        (expect (let ((lister-ignore-filter t))
                  (lister-get-data-at ewoc :last)
                  :to-equal "NEU"))
        (expect (let ((lister-ignore-filter t))
                  (lister--item-visible (ewoc-data
                                      (lister-get-node-at ewoc :last))))
                :not :to-be-truthy))
      (it "inserts and does not hide non-matching item:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-a)
        (lister-add ewoc "BBB")
        (expect (lister-get-data-at ewoc :last)
                :to-equal "BBB")
        (expect (lister--item-visible (ewoc-data
                                    (lister-get-node-at ewoc :last)))
                :to-be-truthy)))))

;; * Sorting

(describe "Sorting:"
  :var (ewoc)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer)))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister--wrap-list"
    (it "wraps a flat list in lists"
      (let ((the-list '(a b c d)))
        (expect (lister--wrap-list the-list)
                :to-equal
                (mapcar #'list the-list))))
    (it "wraps a sublist in the item before it begins"
      (let ((the-list '(a b top (sub-1 sub-2 sub-3))))
        (expect (lister--wrap-list the-list)
                :to-equal
                '((a) (b) (top (sub-1) (sub-2) (sub-3))))))
    (it "wraps a cascade of sublists"
      (let ((the-list '(top1 (top2 (top3 (top4 (s1 s2 s3)))))))
        (expect (lister--wrap-list the-list)
                :to-equal
                '((top1 (top2 (top3 (top4 (s1) (s2) (s3))))))))))

  (describe "lister--reorder-wrapped-list"
    (it "sorts a flat list"
      (let ((the-list '((7) (5) (1) (8) (3) (2)))
            (the-fn   (apply-partially #'seq-sort-by #'car #'<)))
;;             (apply-partially #'seq-sort #'<)))
        (expect (lister--reorder-wrapped-list the-list the-fn)
                :to-equal
                '(1 2 3 5 7 8))))
    (it "sorts wrapped nested lists"
      (let ((the-list  '((8) (6) (4
                                  (43
                                   (40
                                    (401)
                                    (402)
                                    (408)
                                    (405))
                                   (48)))
                         (1) (9) (7) (3)))
            (the-fn   (apply-partially #'seq-sort-by #'car #'<)))
        (expect (lister--reorder-wrapped-list
                 the-list
                 the-fn)
                :to-equal
                '(1 3 4 (43 (40 (401 402 405 408) 48)) 6 7 8 9)))))

  (describe "lister-reverse-list"
    (it "reverses the whole list:"
      (let ((l '("1" "2" "3")))
        (lister-set-list ewoc l)
        (lister-reverse-list ewoc)
        (expect (lister-get-list ewoc) :to-equal (reverse l))))
    (it "keeps marking state of items:"
      (let ((l '("1" "2" "3")))
        (lister-set-list ewoc l)
        (lister-mark-unmark-at ewoc 0 t)
        (lister-reverse-list ewoc)
        (let ((last-node (lister-get-node-at ewoc :last)))
          (expect (lister-node-marked-p last-node)
                  :to-be-truthy)))))

  (describe "lister-sort-list"
    (it "sorts a flat list:"
      (let ((l '("a" "b" "d" "f" "c")))
        (lister-set-list ewoc l)
        (lister-sort-list ewoc #'string>)
        (expect (lister-get-list ewoc)
                :to-equal (seq-sort #'string> l))))
    (it "sorts a flat list using two comparators"
      (let ((l '([1 "c"] [2 "a"] [3 "b"]
                 [1 "a"] [2 "b"] [3 "a"]
                 [1 "b"] [2 "c"] [3 "c"])))
        (cl-labels ((my-num<    (a b) (<       (aref a 0) (aref b 0)))
                    (my-string< (a b) (string< (aref a 1) (aref b 1))))
          (setq ewoc (lister-setup "*LISTER*" (apply-partially #'format "%s")))
          (lister-set-list ewoc l)
          (lister-sort-list ewoc (list #'my-num< #'my-string<)))
        (expect (lister-get-list ewoc)
                :to-equal
                '([1 "a"] [1 "b"] [1 "c"]
                  [2 "a"] [2 "b"] [2 "c"]
                  [3 "a"] [3 "b"] [3 "c"]))))
    (it "keeps marking state of items:"
      (let ((l '("a" "b" "d" "f" "c")))
        (lister-set-list ewoc l)
        (lister-mark-unmark-at ewoc :first t)
        (lister-sort-list ewoc #'string>)
        (let ((last-node (lister-get-node-at ewoc :last)))
          (expect (lister-node-marked-p last-node)
                  :to-be-truthy)))))

  (describe "lister-sort-sublist-at"
    (it "sorts a sublist:"
      (let ((l '("b" "a" ("bb" "ba" "bc") "c")))
        (lister-set-list ewoc l)
        (lister-sort-sublist-at ewoc 2 #'string>)
        (expect (lister-get-list ewoc)
                :to-equal '("b" "a" ("bc" "bb" "ba") "c")))))
  (describe "lister-sort-sublist-below"
    (it "sorts a sublist:"
      (let ((l '("b" "a" ("bb" "ba" "bc") "c")))
        (lister-set-list ewoc l)
        (lister-sort-sublist-below ewoc 1 #'string>)
        (expect (lister-get-list ewoc)
                :to-equal '("b" "a" ("bc" "bb" "ba") "c"))))))

;; * Update

(describe "Update:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-refresh-at:"
    (it "redraws a changed item:"
      (lister-set-list ewoc l)
      (let ((item (ewoc-data (ewoc-nth ewoc 0))))
        (setf (lister--item-data item) "NEW")
        (lister-refresh-at ewoc :first)
        (expect (ewoc-buffer ewoc)
                :to-have-as-content
                (concat (string-join '("NEW" "B" "C" "D")
                                     "\n")
                        "\n")))))
  (describe "lister-refresh-list"
    (it "redraws a list:"
      (lister-set-list ewoc l)
      (let ((n 0))
        (lister-dolist-nodes (ewoc node)
          (setf (lister--item-data (ewoc-data node)) (number-to-string n))
          (setq n (1+ n)))
        (lister-refresh-list ewoc :first :last)
        (expect (ewoc-buffer ewoc)
                :to-have-as-content
                (concat (string-join '("0" "1" "2" "3") "\n")
                        "\n"))))))


;;; * Marking / Unmarking items

(describe "Marking:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l    (mapcar #'number-to-string
                       (number-sequence 0 10))))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister--update-mark-state"
    (it "adds face property for marked items"
      (lister-set-list ewoc l)
      (let* ((item (ewoc-data (ewoc-nth ewoc 0)))
             (pos  (lister--item-beg item)))
        (setf (lister--item-marked item) t)
        (with-current-buffer (ewoc-buffer ewoc)
          (lister--update-mark-state item)
          (expect (get-text-property pos 'face)
                  :to-equal lister-mark-face-or-property))))
    (it "removes face property for unmarked items"
      (lister-set-list ewoc l)
      (let* ((item (ewoc-data (ewoc-nth ewoc 0)))
             (pos  (lister--item-beg item)))
        (setf (lister--item-marked item) t)
        (with-current-buffer (ewoc-buffer ewoc)
          (lister--update-mark-state item)
          (setf (lister--item-marked item) nil)
          (lister--update-mark-state item)
          (expect (get-text-property pos 'face)
                  :to-be nil)))))

  (describe "lister-mark-unmark-at"
    (it "marks a single item:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-at ewoc :first t)
      (let* ((node (lister-get-node-at ewoc :first))
             (pos  (lister--item-beg (ewoc-data node))))
        (expect (get-text-property pos 'face (ewoc-buffer ewoc))
                :to-be lister-mark-face-or-property)))
    (it "umarks a single item:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-at ewoc :first t)
      (lister-mark-unmark-at ewoc :first nil)
      (let* ((node (lister-get-node-at ewoc :first))
             (pos  (lister--item-beg (ewoc-data node))))
        (expect (get-text-property pos 'face (ewoc-buffer ewoc))
                :to-be nil)))
    (it "does nothing if unmarking an unmarked item:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-at ewoc :first nil)
      (let* ((node (lister-get-node-at ewoc :first))
             (pos  (lister--item-beg (ewoc-data node))))
        (expect (get-text-property pos 'face (ewoc-buffer ewoc))
                :to-be nil))))

  (describe "lister-mark-unmark-list"
    (it "marks the whole list:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-list ewoc :first :last t)
      (let ((marked? t))
        (lister-dolist-nodes (ewoc node)
          (setq marked? (and marked?
                             (lister-node-marked-p node))))
        (expect marked? :to-be-truthy))))

  (describe "lister-get-marked-list"
    (it "returns nil if there is no marked item:"
      (lister-set-list ewoc l)
      (expect (lister-get-marked-list ewoc)
              :to-be nil))
    (it "returns a list of all marked items:"
      (lister-set-list ewoc l)
      (let ((mark? t)
            (acc   nil))
        (lister-dolist (ewoc data :first :last node)
          (when mark?
            (push data acc)
            (lister-mark-unmark-at ewoc node t))
          (setq mark? (not mark?)))
        (expect (lister-get-marked-list ewoc)
                :to-equal (nreverse acc))))
    (it "only returns items marked and visible"
      (lister-set-list ewoc l)
      (lister-mark-unmark-list ewoc :first :last t)
      (let ((filter (lambda (s)
                      (not (string-match-p "8" s)))))
        (lister-set-filter ewoc filter)
        (expect (lister-get-marked-list ewoc)
                :to-equal '("8"))))
    (it "per default ignores indentation:"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc 1 '("SUB1" "SUB2"))
      (lister-mark-unmark-sublist-below ewoc 1 t)
      (lister-mark-unmark-at ewoc 0 t)
      (expect (lister-get-marked-list ewoc)
              :to-equal '("0" "SUB1" "SUB2")))
    (it "optionally returns a not-flattened list:"
      (lister-set-list ewoc l)
      (lister-insert-sublist-below ewoc 1 '("SUB1" "SUB2"))
      (lister-mark-unmark-sublist-below ewoc 1 t)
      (lister-mark-unmark-at ewoc 0 t)
      (expect (lister-get-marked-list ewoc :first :last nil t)
              :to-equal '("0" ("SUB1" "SUB2")))))

  (describe "lister-walk-marked-nodes"
    (it "does nothing if nothing is marked:"
      (lister-set-list ewoc l)
      (let (acc)
        (let ((action-fn (lambda (ewoc node)
                           (push (lister-node-get-data node) acc))))
          (lister-walk-marked-nodes ewoc action-fn))
        (expect acc :to-be nil)))
    (it "walks the marked nodes:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-at ewoc 0 t)
      (lister-mark-unmark-at ewoc 1 t)
      (lister-mark-unmark-at ewoc 3 t)
      (let (acc)
        (let ((action-fn (lambda (ewoc node)
                           (push (lister-node-get-data node) acc))))
          (lister-walk-marked-nodes ewoc action-fn))
        (expect acc :to-equal '("3" "1" "0")))))

  (describe "lister-walk-marked-list"
    (it "walks all marked items:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-list ewoc :first :last t)
      (let* (acc
             (action-fn (lambda (data) (push data acc))))
        (lister-walk-marked-list ewoc action-fn)
        (expect (nreverse acc)
                :to-equal (lister-get-marked-list ewoc)))))

  (describe "lister-delete-marked-list"
    (it "does nothing if nothing is marked:"
      (lister-set-list ewoc l)
      (lister-delete-marked-list ewoc)
      (expect (lister-get-list ewoc)
              :to-equal l))
    (it "deletes marked items:"
      (lister-set-list ewoc l)
      (lister-mark-unmark-at ewoc 0 t)
      (lister-mark-unmark-at ewoc 2 t)
      (lister-mark-unmark-at ewoc 4 t)
      (lister-delete-marked-list ewoc)
      (expect (lister-get-list ewoc)
              :to-equal '("1" "3" "5" "6" "7" "8" "9" "10"))))

  (describe "lister-set-marking-predicate"
    (it "sets the buffer local predicate"
      (lister-set-list ewoc l)
      (lister-set-marking-predicate ewoc #'identity)
      (expect (with-current-buffer (ewoc-buffer ewoc)
                lister-local-marking-predicate)
              :to-equal #'identity))
    (it "keeps all previous marks if set to nil"
      (lister-set-list ewoc l)
      (lister-dolist-nodes (ewoc node)
        (lister-mark-unmark-at ewoc node t))
      (lister-set-marking-predicate ewoc nil)
      (expect (lister-get-marked-list ewoc)
              :to-equal l))
    (it "keeps marks which match the new predicate"
      (lister-set-list ewoc l)
      (lister-dolist-nodes (ewoc node)
        (lister-mark-unmark-at ewoc node t))
      (let ((pred (lambda (data)
                    (cl-evenp (string-to-number data)))))
        (lister-set-marking-predicate ewoc pred)
        (expect (lister-get-marked-list ewoc)
                :to-equal (seq-filter pred l)))))

  (describe "lister-mark-unmark-at with marking predicate"
    (it "only allows marking of matching items"
      (lister-set-list ewoc l)
      (let ((pred (lambda (data)
                    (cl-evenp (string-to-number data)))))
        (lister-set-marking-predicate ewoc pred)
        (lister-dolist-nodes (ewoc node)
          (lister-mark-unmark-at ewoc node t))
        (expect (lister-get-marked-list ewoc)
                :to-equal (seq-filter pred l))))))

(describe "Interactive Editing"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("0" "1" "2" ("3" "4" "5") "6" "7" "8")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-save-current-node"
    (it "leaves point at node when other nodes are changed:"
      (lister-set-list ewoc '("0" "1" "2"))
      (with-current-buffer (ewoc-buffer ewoc)
        (let* ((node (lister-get-node-at ewoc 2))
               (_    (lister-goto ewoc node))
               (p    (marker-position (lister--item-beg (ewoc-data node)))))
          (lister-save-current-node ewoc
            (lister-delete-at ewoc 1)
            (lister-insert-at ewoc 1 "1"))
          (expect (point) :to-equal p))))
    (it "follows NODE when it moves:"
      (lister-set-list ewoc '("0" "1" "2"))
      (with-current-buffer (ewoc-buffer ewoc)
        (let* ((node (lister-get-node-at ewoc 2)))
          (lister-goto ewoc node)
          (lister-save-current-node ewoc
            (lister-insert-list ewoc 2 '("NEU" "NEW" "NEUF")))
          (expect (point) :to-equal (marker-position (lister--item-beg (ewoc-data node)))))))
    (it "leaves point untouched when item is gone:"
      (lister-set-list ewoc '("0" "1" "2" "3" "4" "5"))
      (lister-goto ewoc :first)
      (with-current-buffer (ewoc-buffer ewoc)
        (let* ((node (lister-get-node-at ewoc 2))
               (_    (lister-goto ewoc node))
               (p    (point)))
          (lister-save-current-node ewoc
            (lister-delete-at ewoc node))
          (expect (point) :to-be p))))
    (it "keeps point at footer:"
      (lister-set-list ewoc '("0" "1" "2" "3" "4"))
      (lister-set-footer ewoc "FOOTER")
      (with-current-buffer (ewoc-buffer ewoc)
        (ewoc-goto-node ewoc (ewoc--footer ewoc))
      (lister-save-current-node ewoc
          (lister-set-list ewoc '("NEW" "StuFF")))
      (expect (lister-eolp) :to-be-truthy)
      (expect (with-current-buffer (ewoc-buffer ewoc)
                (eobp)) :not :to-be-truthy)))
    (it "keeps point at eob:"
      (lister-set-list ewoc '("0" "1" "2" "3" "4"))
      (lister-set-footer ewoc "FOOTER")
      (with-current-buffer (ewoc-buffer ewoc)
        (goto-char (point-max)))
      (lister-save-current-node ewoc
          (lister-set-list ewoc '("NEW" "StuFF")))
      (expect (lister-eolp) :to-be-truthy)
      (expect (with-current-buffer (ewoc-buffer ewoc)
                (eobp)) :to-be-truthy)))

  (describe "lister--next-node-same-level"
    (it "finds the next node with the same level:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (let ((expected (lister-get-node-at ewoc 2)))
        (expect (lister--next-node-same-level ewoc 0 #'ewoc-next)
                :to-be-node expected)))
    (it "finds the prev node with the same level:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (let ((expected (lister-get-node-at ewoc 0)))
        (expect (lister--next-node-same-level ewoc 2 #'ewoc-prev)
                :to-be-node expected)))
    (it "finds the next node within a sublist:"
      (lister-set-list ewoc '("0" ("1" "2" "3")))
      (let ((expected (lister-get-node-at ewoc 3)))
        (expect (lister--next-node-same-level ewoc 2 #'ewoc-next)
                :to-be-node expected)))
    (it "finds the prev node within a sublist:"
      (lister-set-list ewoc '("0" ("1" "2" "3")))
      (let ((expected (lister-get-node-at ewoc 1)))
        (expect (lister--next-node-same-level ewoc 2 #'ewoc-prev)
                :to-be-node expected)))
    (it "find no node:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (expect (lister--next-node-same-level ewoc 1 #'ewoc-next)
              :to-be nil)))

  (describe "lister--move-item"
    (it "throws an error if no movement is possible - down:"
      (lister-set-list ewoc '("0"))
      (expect (lister--move-item ewoc 0 #'ewoc-next nil nil)
              :to-throw))
    (it "throws an error if no movement is possible - up:"
      (lister-set-list ewoc '("0"))
      (expect (lister--move-item ewoc 0 #'ewoc-prev t nil)
              :to-throw))
    (it "moves item up:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (lister--move-item ewoc 2 #'ewoc-prev t nil)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "2" ("1"))))
    (it "moves item down:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (lister--move-item ewoc 1 #'ewoc-next nil nil)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "2" ("1"))))
    (it "preserves mark when moving down:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (lister-mark-unmark-at ewoc 1 t)
      (lister--move-item ewoc 1 #'ewoc-next nil nil)
      (expect (lister-node-marked-p (lister-get-node-at ewoc 2))
              :to-be-truthy))
    (it "preserves mark when moving up:"
      (lister-set-list ewoc '("0" ("1") "2"))
      (lister-mark-unmark-at ewoc 2 t)
      (lister--move-item ewoc 2 #'ewoc-prev t nil)
      (expect (lister-node-marked-p (lister-get-node-at ewoc 1))
              :to-be-truthy))
    (it "skips sublists when moving up:"
      (lister-set-list ewoc '("0" ("1" "2") "3"))
      (lister--move-item ewoc 3 #'ewoc-prev t t)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "3" ("1" "2"))))
    (it "skips sublists when moving down:"
      (lister-set-list ewoc '("0" ("1" "2") "3"))
      (lister--move-item ewoc 0 #'ewoc-next nil t)
      (expect (lister-get-list ewoc)
              :to-equal '(("1" "2") "0" "3")))
    (it "skips filtered items:"
      (lister-set-list ewoc '("0" "1" "2" "A" "4" "5"))
      (lister-set-filter ewoc (apply-partially #'string-match-p "A"))
      (lister--move-item ewoc 4 #'lister--prev-visible-node t nil)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "1" "4" "A" "2" "5"))))

  (describe "lister-move-item-up"
    (it "throws an error if no movement is possible:"
      (lister-set-list ewoc '("0"))
      (expect (lister-move-item-up ewoc 0)
              :to-throw))
    (it "moves within same level per default:"
      (lister-set-list ewoc '("0" ("1" "1" "1") "2"))
      (lister-move-item-up ewoc :last)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "2" ("1" "1" "1"))))
    (it "optionally ignores level when moving:"
      (lister-set-list ewoc '("0" ("1" "1" "1") "2"))
      (lister-move-item-up ewoc :last t)
      (expect (lister-get-list ewoc)
              :to-equal '("0" ("1" "1") "2" ("1")))))

  (describe "lister-move-item-down"
    (it "throws an error if no movement is possible:"
      (lister-set-list ewoc '("0"))
      (expect (lister-move-item-down ewoc 0)
              :to-throw))
    (it "moves within same level per default:"
      (lister-set-list ewoc '("0" ("1" "1" "1") "2"))
      (lister-move-item-down ewoc :first)
      (expect (lister-get-list ewoc)
              :to-equal '(("1" "1" "1") "0" "2")))
    (it "optionally ignores level when moving:"
      (lister-set-list ewoc '("0" ("1" "1" "1") "2"))
      (lister-move-item-down ewoc :first t)
      (expect (lister-get-list ewoc)
              :to-equal '(("1") "0" ("1" "1") "2"))))

  (describe "lister-move-item-right"
    (it "indents item by one:"
      (lister-set-list ewoc '("0"))
      (lister-move-item-right ewoc :first)
      (expect (lister-get-level-at ewoc :first)
              :to-be 1))
    (it "indents level by two if called two times:"
      (lister-set-list ewoc '("0"))
      (lister-move-item-right ewoc :first)
      (lister-move-item-right ewoc :first)
      (expect (lister-get-level-at ewoc :first)
              :to-be 2)))

  (describe "lister-move-item-left"
    (it "dedents item by one:"
      (lister-set-list ewoc '(("0")))
      (lister-move-item-left ewoc :first)
      (expect (lister-get-level-at ewoc :first)
              :to-be 0))
    (it "dedents level by two if called two times:"
      (lister-set-list ewoc '((("0"))))
      (lister-move-item-left ewoc :first)
      (lister-move-item-left ewoc :first)
      (expect (lister-get-level-at ewoc :first)
              :to-be 0)))

  (describe "lister-move-sublist-up"
    (it "throws an error if no movement is possible:"
      (lister-set-list ewoc '("0" ("A" "B")))
      (expect (lister-move-sublist-up ewoc 2)
              :to-throw))
    (it "moves sublist up:"
      (lister-set-list ewoc '("0" "1" ("A" "B")))
      (lister-move-sublist-up ewoc 2)
      (expect (lister-get-list ewoc)
              :to-equal '("0" ("A" "B") "1")))
    (it "preserves marking state:"
      (lister-set-list ewoc '("0" "1" ("A" "B")))
      (lister-mark-unmark-sublist-at ewoc 2 t)
      (lister-move-sublist-up ewoc 2)
      (let ((n1 (lister-get-node-at ewoc 1)) ;; "A"
            (n2 (lister-get-node-at ewoc 2))) ;; "B"
        (expect (lister-node-marked-p n1) :to-be-truthy)
        (expect (lister-node-marked-p n2) :to-be-truthy))))

  (describe "lister-move-sublist-down"
    (it "throws an error if no movement is possible:"
      (lister-set-list ewoc '("0" ("A" "B")))
      (expect (lister-move-sublist-down ewoc 2)
              :to-throw))
    (it "moves sublist down:"
      (lister-set-list ewoc '("0" ("A" "B") "1"))
      (lister-move-sublist-down ewoc 2)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "1" ("A" "B"))))
    (it "preserves marking state:"
      (lister-set-list ewoc '("0" ("A" "B") "1"))
      (lister-mark-unmark-sublist-at ewoc 2 t)
      (lister-move-sublist-down ewoc 2)
      (let ((n1 (lister-get-node-at ewoc 2)) ;; "A"
            (n2 (lister-get-node-at ewoc 3))) ;; "B"
        (expect (lister-node-marked-p n1) :to-be-truthy)
        (expect (lister-node-marked-p n2) :to-be-truthy))))

  (describe "lister-move-sublist-left"
    (it "throws an error if no movement is possible:"
      (lister-set-list ewoc '("0" "A" "B"))
      (expect (lister-move-sublist-left ewoc 1)
              :to-throw))
    (it "moves sublist left:"
      (lister-set-list ewoc '("0" ("A" "B")))
      (lister-move-sublist-left ewoc 1)
      (expect (lister-get-list ewoc)
              :to-equal '("0" "A" "B"))))

  (describe "lister-move-sublist-right"
    (it "moves sublist right:"
      (lister-set-list ewoc '("0" ("A" "B")))
      (lister-move-sublist-right ewoc 1)
      (expect (lister-get-list ewoc)
              :to-equal '("0" (("A" "B")))))))

;; -----------------------------------------------------------
;; * Tests for Lister Mode
;; -----------------------------------------------------------

(describe "Lister mode"
  (it "disables view mode if `view-read-only' is set"
    (let* ((view-read-only t)
           (ewoc (lister-test-setup-minimal-buffer))
           (buf  (ewoc-buffer ewoc)))
      (with-current-buffer buf
        (lister-mode +1)
        (expect lister-mode :to-be-truthy)
        (expect view-mode :not :to-be-truthy))
      (kill-buffer buf))))


(provide 'lister-tests)
;;; lister-tests.el ends here

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
