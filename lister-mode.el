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

(defun lister-mode--push-goto (ewoc pos)
  "If point is not at POS, maybe push mark and move point.
Push mark only if POS is not an immediate visible neighbour of
the current node.  EWOC is a Lister Ewoc Object."
  (lister-with-node ewoc pos node
    (let ((current (lister--parse-position ewoc :point)))
      (unless (equal node current)
        (unless (or (equal node (lister--next-node-matching ewoc current #'lister-node-visible-p #'ewoc-next))
                    (equal node (lister--next-node-matching ewoc current #'lister-node-visible-p #'ewoc-prev)))
          (push-mark))
        (lister-goto ewoc node)))))

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
    ('nil   (progn
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

;; Move sublists up or down

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

;; Move within the hierarchy

(defun lister-mode--next-visible-node-same-level (ewoc node move-fn)
  "Find the next visible node with the same level as NODE.
EWOC is a Lister Ewoc Object.  MOVE-FN must be either `ewoc-next'
or `ewoc-prev'."
  (let* ((ref-level (lister-node-get-level node)))
    (lister--next-node-matching ewoc node
                                (lambda (n)
                                  (and (lister-node-visible-p n)
                                       (= ref-level (lister-node-get-level n))))
                                move-fn)))

(lister-defkey lister-mode-backward-same-level (ewoc pos prefix node)
  "Move backward to the next item with the same level."
  (if-let ((next (lister-mode--next-visible-node-same-level ewoc node #'ewoc-next)))
      (lister-mode--push-goto ewoc next)
    (user-error "No further item with the same level")))

(lister-defkey lister-mode-forward-same-level (ewoc pos prefix node)
  "Move forward to the next item with the same level."
  (if-let ((next (lister-mode--next-visible-node-same-level ewoc node #'ewoc-prev)))
      (lister-mode--push-goto ewoc next)
    (user-error "No further item with the same level")))

(lister-defkey lister-mode-up-parent (ewoc pos prefix node)
  "Move up to the parent node."
  (if-let ((parent (lister-parent-node ewoc pos)))
      (lister-mode--push-goto ewoc parent)
    (user-error "No parent found")))

(defun lister-mode--outer-top-sublist-node (ewoc pos)
  "In EWOC, find the outer top sublist node above POS.
Return the node or nil if no such node is available."
  (when-let ((parent (lister-parent-node ewoc pos)))
    (lister-top-sublist-node ewoc parent)))

(defun lister-mode--outer-bottom-sublist-node (ewoc pos)
  "In EWOC, find the outer bottom sublistnode below POS.
Return the node or nil if no such node is available."
  (lister-with-sublist-at ewoc pos beg end
    (when-let* ((outer-first (ewoc-next ewoc end))
                (outer-bounds (lister--locate-sublist ewoc outer-first t)))
      (nth 1 outer-bounds))))

(lister-defkey lister-mode-goto-beginning (ewoc pos prefix node)
  "Move up to the beginning of the sublist or list.
Call this function several times to move out of nested sublists."
  (let ((top (lister-top-sublist-node ewoc pos)))
    (when (equal node top)
      (setq top (lister-mode--outer-top-sublist-node ewoc top)))
    (if top
        (lister-mode--push-goto ewoc top)
      (if-let ((first (lister--next-or-this-node-matching ewoc (lister-get-node-at ewoc :first)
                                                         #'lister-node-visible-p
                                                         #'ewoc-next
                                                         node)))
          (lister-mode--push-goto ewoc first)
        (user-error "Beginning of list")))))

(lister-defkey lister-mode-goto-end (ewoc pos prefix node)
  "Move down to the end of the sublist or list.
Call this function several times to move out of nested sublists."
  (let ((bottom (lister-bottom-sublist-node ewoc pos)))
    (when (equal node bottom)
      (setq bottom (lister-mode--outer-bottom-sublist-node ewoc bottom)))
    (if bottom
        (lister-mode--push-goto ewoc bottom)
      (if-let ((last (lister--next-or-this-node-matching ewoc (lister-get-node-at ewoc :last)
                                                         #'lister-node-visible-p
                                                         #'ewoc-prev
                                                         node)))
          (lister-mode--push-goto ewoc last)
        (user-error "End of list")))))

(lister-defkey lister-mode-next-child-forward (ewoc pos prefix node)
  "Move to the next child looking up from point."
  (if-let ((next (lister-first-sublist-node ewoc pos 'up)))
      (lister-mode--push-goto ewoc next)
    (user-error "No child found above point")))

(lister-defkey lister-mode-next-child-backward (ewoc pos prefix node)
  "Move to the next child looking down from point."
  (if-let ((next (lister-first-sublist-node ewoc pos 'down)))
      (lister-mode--push-goto ewoc next)
    (user-error "No child found below point")))

;; * The Keymap
(defvar lister-mode-map
  (let ((map (make-sparse-keymap)))
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
    ;;
    (define-key map [remap beginning-of-buffer] 'lister-mode-goto-beginning)
    (define-key map [remap end-of-buffer]       'lister-mode-goto-end)
    (define-key map (kbd "C-c C-u")             'lister-mode-up-parent)
    (define-key map (kbd "C-c C-f")             'lister-mode-forward-same-level)
    (define-key map (kbd "C-c C-b")             'lister-mode-backward-same-level)
    (define-key map (kbd "C-c <C-up>")          'lister-mode-next-child-forward)
    (define-key map (kbd "C-c <C-down>")        'lister-mode-next-child-backward)
    map)
  "Key map for `lister-mode'.")

(define-minor-mode lister-mode
  "Bind some keys for basic operations in a lister buffer."
  :lighter "lister "
  :group 'lister
  :keymap 'lister-mode-map)

(provide 'lister-mode)
;;; lister-mode.el ends here
