;;; lister-mode.el --- Key definitions for Lister buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/publicimageltd/lister

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

;; This minor mode adds some keymaps for lister buffers.

;;; Code:
(require 'lister)

;; Silence compiler warnings
(defvar lister-local-ewoc)

;; Lazy programmers do not want to type so much.
(cl-defmacro lister-defkey
    (fn-name (ewoc-var pos-var prefix-var node-var)
             docstring
             &rest body)
  "Wrap BODY in an interactive defun to be used as a key.
Define the function using FN-NAME, giving it the documentation
string DOCSTRING.  When BODY is called in this function, let
EWOC-VAR be bound to the buffer local ewoc object, POS-VAR to the
symbol `:point', PREFIX-VAR to the current prefix arg and
NODE-VAR to the node at point."
  (declare (indent 2)
           (debug (&define name (symbolp symbolp symbolp symbolp)
                     stringp def-body))
           (doc-string 3))
  `(defun ,fn-name (,ewoc-var ,pos-var &optional ,prefix-var)
     ,(concat docstring
              "\nIf called interactively, EWOC will be bound to
the buffer local ewoc object and POS refers to the item at
point.")
     (interactive (list
                   (or lister-local-ewoc (error "Key only works in a lister buffer"))
                   :point
                   current-prefix-arg))
     (ignore ,prefix-var ,pos-var ,ewoc-var)
     (let ((,node-var (lister--parse-position ewoc pos)))
       (if (not ,node-var)
           (error "No item at indicated position")
         ,@body))))

;;; * Key Definitions

;; Mark or unmark the item at point

(defun lister-mode--generic-mark (ewoc pos prefix state)
  "Mark or unmark item or sublist.
In EWOC, mark or unmark the item at POS according to STATE and
move forward one item.  If PREFIX is '(4), mark the sublist at
POS.  If PREFIX is '(16), mark the sublist below POS."
  (pcase prefix
    (`(4)  (lister-mark-unmark-sublist-at ewoc pos state))
    (`(16) (lister-mark-unmark-sublist-below ewoc pos state))
    (null   (progn
             (lister-mark-unmark-at ewoc pos state)
             (lister-goto ewoc :next)))))

(defun lister-mode--mark-unmark-region (ewoc from to state)
  "Mark region from buffer positions FROM and TO according to STATE.
EWOC is a lister ewoc object."
  (with-current-buffer (ewoc-buffer ewoc)
    (let* ((beg (ewoc-locate ewoc from))
           (end (ewoc-locate ewoc to)))
      (when (= (ewoc-location end) to)
        (setq end (ewoc-prev ewoc end)))
      (lister-dolist-nodes (ewoc node beg end)
        (lister-mark-unmark-at ewoc node state)))))

(lister-defkey lister-mode-mark (ewoc pos prefix node)
  "Mark the item or sublist at point or the items in the region.
Use EWOC and POS to determine the item to be marked.  If PREFIX
is non-nil, mark the sublist at POS.  If the region is active,
unmark all items in the region and ignore PREFIX."
  (if (and (use-region-p)
           (not (region-noncontiguous-p)))
      (lister-mode--mark-unmark-region ewoc
                                       (region-beginning)
                                       (region-end) t)
  (lister-mode--generic-mark ewoc pos prefix t)))

(lister-defkey lister-mode-unmark (ewoc pos prefix node)
  "Unmark the item or sublist at point or the items in the region.
Use EWOC and POS to determine the item to be marked.  If PREFIX
is non-nil, unmark the sublist at POS.  If the region is active,
unmark all items in the region and ignore PREFIX."
  (if (and (use-region-p)
           (not (region-noncontiguous-p)))
      (lister-mode--mark-unmark-region ewoc
                                       (region-beginning)
                                       (region-end)
                                       nil)
  (lister-mode--generic-mark ewoc pos prefix nil)))

(defun lister-mode-unmark-all ()
  "Unmark all items in the current Lister buffer."
  (interactive)
  (unless lister-local-ewoc
    (error "Functions needs to be called in a Lister buffer with an Ewoc object"))
  (lister-walk-marked-nodes lister-local-ewoc
                            (lambda (ewoc node)
                              (lister-mark-unmark-at ewoc node nil))))

;; Cycle subtree visibility

(lister-defkey lister-mode-cycle-sublist (ewoc pos prefix node)
  "Cycle the visibility of the sublist below point.
Use EWOC and POS to determine the sublist to be marked.  If
PREFIX is non-nil, show all sublists."
  (if prefix
      (lister-outline-show-all ewoc)
    (lister-outline-cycle-sublist-below ewoc pos)))

;; Move items up or down

(lister-defkey lister-mode-up (ewoc pos prefix node)
  "Move the item at point one up.
Only move within the same level unless PREFIX is set."
  (lister-move-item-up ewoc pos prefix))

(lister-defkey lister-mode-down (ewoc pos prefix node)
  "Move the item at point one down.
Only move within the same level unless PREFIX is set."
  (lister-move-item-down ewoc pos prefix))

(lister-defkey lister-mode-right (ewoc pos prefix node)
  "Move the item at point to the right."
  (lister-move-item-right ewoc pos))

(lister-defkey lister-mode-left (ewoc pos prefix node)
  "Move the item at point to the left."
  (lister-move-item-left ewoc pos))

;; Move subtlists up or down

(lister-defkey lister-mode-sublist-up (ewoc pos prefix node)
  "Move the sublist at point one up."
  (lister-move-sublist-up ewoc pos))

(lister-defkey lister-mode-sublist-down (ewoc pos prefix node)
  "Move the sublist at point one down."
  (lister-move-sublist-down ewoc pos))

(lister-defkey lister-mode-sublist-right (ewoc pos prefix node)
  "Move the sublist at point to the right."
  (lister-move-sublist-right ewoc pos))

(lister-defkey lister-mode-sublist-left (ewoc pos prefix node)
  "Move the sublist at point to the left."
  (lister-move-sublist-left ewoc pos))

;; * The Keymap
(defvar lister-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "m" 'lister-mode-mark)
    (define-key map "u" 'lister-mode-unmark)
    (define-key map "U" 'lister-mode-unmark-all)
    (define-key map "\t" 'lister-mode-cycle-sublist)
    (define-key map (kbd "<M-up>")      'lister-mode-up)
    (define-key map (kbd "<M-down>")    'lister-mode-down)
    (define-key map (kbd "<M-right>")   'lister-mode-right)
    (define-key map (kbd "<M-left>")    'lister-mode-left)
    (define-key map (kbd "<S-M-up>")    'lister-mode-sublist-up)
    (define-key map (kbd "<S-M-down>")  'lister-mode-sublist-down)
    (define-key map (kbd "<S-M-right>") 'lister-mode-sublist-right)
    (define-key map (kbd "<S-M-left>")  'lister-mode-sublist-left)
    map)
  "Key map for `lister-mode'.")

(define-minor-mode lister-mode
  "Bind some keys for basic operations in a lister buffer."
  :lighter "lister "
  :group 'lister
  :keymap 'lister-mode-map)

(provide 'lister-mode)
;;; lister-mode.el ends here
