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
(require 'buttercup)
(require 'seq)
(require 'cl-lib)
(require 'cl-extra)
(require 'rx)

;; (message "Testing lister version %s on Emacs %s" lister-version emacs-version)

;; (setq buttercup-stack-frame-style 'pretty)

(defvar lister-default-left-margin)

(defun lister-test-setup-minimal-buffer ()
  "Set up a minimal buffer, with no margins and a simple list mapper.
Return the ewoc object"
  (let ((ewoc (lister-setup "*LISTER*" #'list)))
    (with-current-buffer (ewoc-buffer ewoc)
      (setq-local lister-default-left-margin 0))
    ewoc))

(defun lister-test-get-node-at-point (ewoc)
  "Return the EWOC node at point.
Workaround for `ewoc-locate', which does not set the current
buffer if called with pos=nil."
  (with-current-buffer (ewoc-buffer ewoc)
    (ewoc-locate ewoc)))

(defun lister-test-get-data-at-point (ewoc)
  "Return the data of the item at point in EWOC.
This is functionally equivalent to `lister-get-data-at', but uses
low-lewel ewoc functions instead of `lister--parse-position'."
  (lister-get-data-at ewoc (lister-test-get-node-at-point ewoc)))

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

;; to match nodes:
(buttercup-define-matcher :to-be-node (node1 node2)
  (let* ((n1   (funcall node1))
         (n2   (funcall node2))
         (n1-data (when n1 (lister--item-data (ewoc-data n1))))
         (n2-data (when n2 (lister--item-data (ewoc-data n2)))))
    (buttercup--test-expectation
        (equal n1 n2)
      :expect-match-phrase
      (format "Expected node to be %s, but instead it was %s"
              n2-data (if n1 n1-data "no node at all"))
      :expect-mismatch-phrase
      (format "Expected node not to be '%s', but instead it was"
              n2-data))))

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
;; The tests.

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

  (describe "lister-add:"
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
      (expect (lister-get-list ewoc) :to-equal '("1" "2")))))

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
              :to-have-as-content "TEST\n "))
    (it "removes header when called with nil:"
      (lister-set-header ewoc "TEST")
      (lister-set-header ewoc nil)
      (expect (ewoc-buffer ewoc)
              :to-have-as-content " "))
    (it "accepts a list of strings:"
      (lister-set-header ewoc '("TEST" "TEST"))
      (expect (ewoc-buffer ewoc)
              :to-have-as-content "TEST\nTEST\n "))
    (it "adds header before a list:"
      (lister-set-list ewoc l)
      (lister-set-header ewoc "TEST")
      (expect (ewoc-buffer ewoc)
              :to-have-as-content
              (concat "TEST\n"
                      (string-join l "\n")
                      "\n ")))))

;;; * Diverse API

(describe "Diverse API"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("A" "B" "C" "D" "E" "F")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-with-region:"
    (xit "throws an error if not called with symbols"
      ;; This test does not fail, but causes annoying "eager macro
      ;; expansion" error; so we x it
      (expect (lister-with-region ewoc 1 0
                (ignore))
              :to-throw)
      (expect (lister-with-region ewoc :first :last
                (ignore))
              :to-throw))
    (it "passes through nodes"
      (lister-set-list ewoc l)
      (let ((first (ewoc-nth ewoc 0))
            (second (ewoc-nth ewoc 1)))
        (expect (lister-with-region ewoc first second
                  first)
                :to-be-node first)))
      (it "accepts :first, :last as values"
        (lister-set-list ewoc l)
        (let ((first :first)
              (second :last))
          (expect (lister-with-region ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-region ewoc first second
                    second)
                  :to-be-node (ewoc-nth ewoc -1))))
      (it "accepts indices as values"
        (lister-set-list ewoc l)
        (let ((first 0)
              (second (1- (length l))))
          (expect (lister-with-region ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-region ewoc first second
                    second)
                  :to-be-node (ewoc-nth ewoc -1))))
      (it "accepts nil as values"
        (lister-set-list ewoc l)
        (let (first second)
          (expect (lister-with-region ewoc first second
                    first)
                  :to-be-node (ewoc-nth ewoc 0))
          (expect (lister-with-region ewoc first second
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
      (expect (lister-empty-p ewoc) :to-be-truthy)))

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

    (it "identifies a surrounded sublist:"
      (lister-set-list ewoc '("0" "1" ("2" "3" "4" "5") "4" "5"))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 5))))
        (expect (lister--locate-sublist ewoc 2)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 4)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 5)
                :to-equal expected-bounds)))
    (it "identifies a sublist with 'open end':"
      (lister-set-list ewoc '("0" "1"
                           ("2" "3" "4" "5")))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 5))))
        (expect (lister--locate-sublist ewoc 2)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 4)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 5)
                :to-equal expected-bounds)))
    (it "identifies a surrounded nested  sublist:"
      (lister-set-list ewoc '("0" ("1" ("2" "3") "4" "5")))
      (let ((expected-bounds (list (ewoc-nth ewoc 2)
                                   (ewoc-nth ewoc 3))))
        (expect (lister--locate-sublist ewoc 2)
                :to-equal expected-bounds)
        (expect (lister--locate-sublist ewoc 3)
                :to-equal expected-bounds))))

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

;; - lister-goto
  ;; - with-locked-cursor

(describe "Navigation:"
  :var (ewoc l)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l '("0" "1" "2" "3" "4" "5"))
    (lister-set-list ewoc l))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "lister-goto:"
    (it "moves point to the first item of the list:"
      (lister-goto ewoc :first)
      (expect (lister-test-get-data-at-point ewoc)
              :to-equal "0"))
    (it "moves point to the last item of the list:"
      (lister-goto ewoc :last)
      (expect (lister-test-get-data-at-point ewoc)
              :to-equal "5"))
    (it "moves point to the next item:"
      (lister-goto ewoc :first)
      (lister-goto ewoc :next)
      (expect (lister-test-get-data-at-point ewoc)
              :to-equal "1"))
    (it "moves point to the prev item:"
      (lister-goto ewoc :last)
      (lister-goto ewoc :prev)
      (expect (lister-test-get-data-at-point ewoc))
              :to-equal "4"))
    (it "moves point to an index position:"
      (lister-goto ewoc 4)
      (expect (lister-test-get-data-at-point ewoc)
              :to-equal "4"))
    (it "throws an error if index is out of bounds:"
      (expect (lister-goto ewoc 8)
              :to-throw)))

;; * Editing

;; - move-item-up, move-item-down


;;; * Filter

(describe "Filter:"
  :var (ewoc l pred-fn filter-a)
  (before-each
    (setq ewoc (lister-test-setup-minimal-buffer))
    (setq l    '("AZ" "BAZ" "DAZ" "MAZ"
                 ("ITEM4" "ITEM5" "ITEM6")
                 "HEY" "THEY" "OBEY"))
    ;; filters the first 4 items:
    (setq filter-a (apply-partially #'string-match "A"))
    ;; filter everything but the first 4:
    (setq filter-non-a (lambda (s)
                         (not (string-match "A" s))))
    ;; filters the last 3 items:
    (setq filter-b (apply-partially #'string-match "Y"))
    ;; filters the sublist:
    (setq filter-i (apply-partially #'string-match "ITEM"))
    ;; filters everything but the sublist:
    (setq filter-non-i (lambda (s)
                         (not (funcall filter-i s))))
    ;; filters everything:
    (setq filter-* (apply-partially #'string-match ".*")))
  (after-each
    (kill-buffer (ewoc-buffer ewoc)))

  (describe "Movement while filter is OFF:"
    (describe "lister--next-visible-node:"
      (it "moves to the next node:"
        (lister-set-list ewoc l)
        (let ((node (ewoc-nth ewoc 0)))
          (expect (lister--next-visible-node ewoc node)
                  :to-be-node (ewoc-nth ewoc 1))))
      (it "moves to prev node:"
        (lister-set-list ewoc l)
        (let ((node (ewoc-nth ewoc -1)))
          (expect (lister--next-visible-node ewoc node #'ewoc-prev)
                  :to-be-node (ewoc-nth ewoc -2))))
      (it "returns nil if already at last node:"
        (lister-set-list ewoc l)
        (let ((node (ewoc-nth ewoc -1)))
          (expect (lister--next-visible-node ewoc node)
                  :to-be nil)))
      (it "returns nil if already at first node:"
        (lister-set-list ewoc l)
        (let ((node (ewoc-nth ewoc 0)))
          (expect (lister--next-visible-node ewoc node #'ewoc-prev)
                  :to-be nil))))

    (describe "lister--first-visible-node:"
      (it "returns first node:"
        (lister-set-list ewoc l)
        (expect (lister--first-visible-node ewoc)
                :to-be-node (ewoc-nth ewoc 0))))

    (describe "lister--last-visible-node:"
      (it "returns last node:"
        (lister-set-list ewoc l)
        (expect (lister--last-visible-node ewoc)
                :to-be-node (ewoc-nth ewoc -1)))))

  (describe "Hide visible items:"
    (describe "lister-set-filter:"
      (it "hides all items:"
        (lister-set-list ewoc l)
        (lister-set-filter ewoc filter-*)
        (expect (ewoc-buffer ewoc)
                :to-have-as-visible-content " "))))

  (describe "Movement while filter is ON:"
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
                :to-be nil))))

  (describe "retrieving lists with filter ON:"
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

  ;; - with-locked-cursor
  ;;     "keeps the visual line if the item is filtered away in its body"
  ;;    "jumps to the last visible line if the current line has been filtered away in the body"
  ;;    "jumps to the first line if the body filters all items"


;; * Levels and indentation

  ;; - insert
  ;;   - does not allow indentation > 0 at top
  ;;   - inherits indentation from previous item
  ;;   - does not allow indentation difference > 1 at non-top item
  ;;  - replace
  ;;    - can chang indentation level


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
        (expect (lister-get-list ewoc) :to-equal (reverse l)))))

  (describe "lister-sort-list"
    (it "sorts a flat list:"
      (let ((l '("a" "b" "d" "f" "c")))
        (lister-set-list ewoc l)
        (lister-sort-list ewoc #'string>)
        (expect (lister-get-list ewoc)
                :to-equal (seq-sort #'string> l)))))
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
                        "\n ")))))
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
                        "\n "))))))


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
              :to-equal '("1" "3" "5" "6" "7" "8" "9" "10")))))

(provide 'lister-tests)
;;; lister-tests.el ends here

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
