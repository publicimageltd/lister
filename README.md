# Lister - Yet Another List Printer for Emacs

## What is it for?

`Lister` is a library for creating interactive "lists" of any kind. In
contrast to similar packages like `hierarchy.el` or `tablist.el`, it
aims at *not* simply mapping a data structure to a navigatable list.
Rather, it treats the list like emacs treats buffers: It is an empty
space to which you can successively add stuff. So in emacs lingo,
`lister` should be rather called `listed` - it is a library for
*editing* lists, instead of displaying them.

The advantage of this approach is that you don't have to know in
advance the structure of your data. You just "insert" a new slice on
the fly when you need it.

Lister is currently only used for `delve.el`, another project of mine.
`Delve` is a good usecase for working with lists the lister-style: You
navigate your `org-roam` Zettelkasten by following the links of each
page. These links, however, do quickly become cyclic: One page links
to another, and that one links back to the first one. With lister,
these cycles are no problem. The user just inserts another sublist at
point if he or she wants to dive a bit deeper into the structure of a
Zettelkasten page.

## Project still in progress

`Lister` does work. I use it every day. Yet at the beginning, I had
implemented quite some features which I did not debug anymore, and
which are not properly tested. So better don't use filtering, for
example. I am planning, of couse, to weed out all that stuff in some
closer future. But since I have next to no time at all for this kind
of work, it will take some time.

## Some basic facts

I don't think there will be too many people actually using this
library. But since I use it myself for `delve`, it makes sense to
document some of the features and some of the API.

### Design principles

 - A "list" is a collection of printed items. The items do not have to
   be of the same size. The data associated with the item is stored as
   a text property.
 - Every item is printed by passing to a buffer local mapper function.
   This function accepts the data and returns a list of printable
   lines. It is possible to add a margin to each item.
 - Each item has its own "indentation level", making it possible to
   print and operate on nested lists ("sublists").
 - Navigation is done using Emacs' regular navigation commands. Each
   printed item is inserted with a cursor gap, on which alone point
   can rest. (The rest of the item is marked as intangible.) 
 - For the sake of speed, there is a buffer local list of marker
   positions to keep track of each item (`lister-local-marker-list`).
   So jumping to an item is as fast as jumping to the associated
   marker. If you move a list item, the marker will be moved with it.
 - There are hooks defined for responding to the events of entering an
   item or leaving it. This is used for implementing a minor mode
   which highlights the item at point and, of course, removes the
   highlihting on leaving it (`lister-highlight-mode`).
 
### Example 

``` emacs-lisp
(let* ((buf (generate-new-buffer "*LISTER*")))
  (lister-setup buf 
                #'list ;; mapper-fn
		'("3" "4" "5" "6") ;; initial data
		"This is my list:" ;; header
		"This is the end of the list") ;; footer
  (lister-goto buf :first) ;; move to first item
  (lister-insert buf :point "2")
  (lister-insert buf :point "1")
  (switch-to-buffer buf))				
```

This little program yields:

```
  This is my list:
  1
  2
  3
  4
  5
  6
  This is the end of the list
```

### Doing something with an item

If you press <ENTER> on an item, the function `lister-local-action` is
called with the associated data as its sole argument. So if you want
something to happen, bind this buffer local variable to something
useful. Toy example:

``` emacs-lisp
(defun my-action (data)
   (message "You just pressed enter on the item %s." data))

;; install the action somewhere after you set up the buffer:
(with-current-buffer the-lister-buffer
  (setq lister-local-action #'my-action))
```

### lister mode

Each lister buffer has the major mode `lister-mode`. It offers the
following keybindings:

``` emacs-lisp
    ;; untested marking:
    (define-key map "m" 'lister-key-toggle-mark)     
    (define-key map "*" 'lister-key-mark-all-items)
    (define-key map "u" 'lister-key-unmark-all-items)
	;; basic navigation:
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
	;; press RET to do something:
    (define-key map (kbd "RET") #'lister-key-action)
```

### Overriding lister mode

If you want to derive your own major mode from lister mode, you might
want to provide an initial list. Since `lister-setup` also turns on
lister mode, you have to call it with an optional argument telling it
to *not* turn on lister mode:

``` emacs-lisp
(define-derived-mode some-mode
  lister-mode "something"
  "Some description of your own major mode."
  ;; Setup lister, but don't set the major mode:
  (lister-setup	(current-buffer) 
        #'some-mode--mapper-fn
		nil
		nil ;; header
		nil ;; footer
		nil ;; filter
		t   ;; no major-mode <--- !!!
		)
  ;; Now add mode specific stuff:
  (setq-local lister-local-action #'some-mode-action)) 
```

### Functions and Variables

A short list of the most important functions -- have a look at
the documentation strings if you want to know more:

 - lister-setup : Initializes a buffer and turns on "lister-mode".
 - lister-set-list : Remove the current list or replace it with
   another list.
 - Editing lists:
   - lister-insert
   - lister-insert-sequence
   - lister-insert-sublist-below
   - lister-add
   - lister-add-sequence
   - lister-remove
   - lister-remove-this-level
   - lister-remove-sublist-below
   - lister-replace
  - Navigation:
	- lister-goto 
 - Accessing the data slot:
	-  lister-get-data
	- lister-set-date
	- lister-get-all-data
	- lister-get-all-data-tree
  
  Further high-level functions are dealing with filtering, and there
  is also a basic facility to mark an item and to process these
  selected items. But this is not tested well.

# Plans for the future

Apart from the more abstract plan to weed out all unused stuff which
clutters the name space, I have the following plans:

 - [ ] Write tests for marking items and working with marked items.
 - [ ] Test the filtering
 - [ ] Change the rather clumsy logic by which an "action" is
 triggered on an item.
