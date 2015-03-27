# helm-browse
multi-purpose searching framework for helm


## Summary

This is the prototype of a multi-purpose searching framework for helm.
It focuses on speed and extensibility.  It doesn't rely on any
external programs like grep.

Multi-file searches and editing of matches are not yet implemented.


## Requirements

Emacs 25.

iterators.el, available from Gnu Elpa or from here:

  https://github.com/michael-heerdegen/iterators.el


## Installation

Put iterators.el and helm-browse.el in your load-path and byte compile them.


## Usage

### helm-browse-lines

The main command is "helm-browse-lines".

 - given regexps are matched against the current buffer's lines

 - you can negate a pattern by prepending a "!"

 - There is a "-n" switch allowing matches spanning multiple lines.
   If you give "-n3", candidates are allowed to span up to three
   lines.  If you specify just "-n", candidates may span any number of
   lines, but not accross empty lines.  This is useful to search
   within the defuns of some source code buffer, or the paragraphs of
   a text buffer.

There are lots of other search commands implemented:


### helm-browse-outline

browse the headings in a buffer using outlines


### helm-browse-diff

browse changes in a file's buffer not yet committed.  Requires library
diff-hl.el.


### helm-browse-dired

A dired search and marking tool.

- You match against the file names as they appear in the buffer.

- Actions to mark and unmark matching files

- Lots of switches are supported:

  + "-l" matches symlinks.  "-lname" matches symlinks with target
    matching NAME

  + "-d" matches directories

  + "-apath" matches PATH against the absolute file name

  + "-m*" matches marked files.  "-mc" matches files marked with
  character C.  Just "-m" matches files marked with any character

  + "-r" matches regular files

  + "-s>size", "-s<size" matches files with size < or > SIZE,
  e.g. "-s>2m" matches files with size larger than 2 megabytes

  + "-h=n" "-h>n" "-h<n" matches N against number of hardlinks

  + All flags can be negated, e.g. "!-d" matches anything that is not
  a directory etc.


### helm-browse-w3m-links

Browse the links in a w3m buffer.


### helm-browse-eww-links

Browse links in an eww buffer.


### helm-browse-comint-inputs

Browse input strings in a comint (or shell etc.) buffer.
