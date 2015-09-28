;; multi-purpose searching framework for helm  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Michael Heerdegen

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2015_03_26
;; URL: https://github.com/michael-heerdegen/helm-browse
;; Keywords: matching
;; Compatibility: GNU Emacs >=24
;; Version: 0.1
;; Package-Requires: ()


;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; The prototype of a multi-purpose searching framework for helm
;;
;; To do:
;;
;; - implement multi file searching
;; - implement match editing à la wgrep


;;; Code:


(require 'generator)
(require 'iterators)


(defgroup helm-browse nil
  "Doc..."
  :group 'matching
  :prefix "helm-browse")

(defface helm-browse-highlight-match
  '((t (:inherit 'match)))
  "Doc..." :group 'helm-browse)

(defface helm-browse-this-cand
  '((t (:inherit 'helm-selection)))
  "Doc..." :group 'helm-browse)

(defface helm-browse-cand-region
  '((((background light)) :background "gray50")
    (t (:background "#173719")))
  "Doc..." :group 'helm-browse)


;; underlying stuff

(defmacro helm-browse--delay-expression (expr)
  ;;  Return a delayed object evaluating EXPR.
  ;; `funcall' the return value to force evaluation.  Successive
  ;; `funcall's will just return the same result without evaluating
  ;; EXPR again.
  (let ((todo   (make-symbol "todo"))
	(result (make-symbol "result")))
    `(let (,result (,todo (lambda () ,expr)))
       (lambda () (if (not ,todo) ,result
	       (setq ,result (funcall ,todo)
		     ,todo  nil)
	       ,result)))))

(defun helm-browse--search-forward-regexp (regexp &rest args)
  "`search-forward-regexp' but respect `helm-case-fold-search' and catch `invalid-regexp' errors."
  (let ((case-fold-search (let ((case-fold-search nil))
                            (pcase helm-case-fold-search
                              (`smart (not (string-match-p "[[:upper:]]" regexp)))
                              (_      case-fold-search)))))
    (condition-case nil
        (apply #'search-forward-regexp regexp args)
      (invalid-regexp nil))))

(defun helm-browse-after-selection-move ()
  (when (memq this-command '(helm-next-line
                             helm-previous-line
                             helm-end-of-buffer
                             helm-beginning-of-buffer))
    (helm-browse-default-goto (helm-get-selection))))

(defun helm-browse-prepair-hooks ()
  (add-hook 'helm-after-update-hook #'helm-browse-move-to-first-cand-after-point)
  (add-hook 'helm-move-selection-after-hook #'helm-browse-after-selection-move))

(defun helm-browse-restore-hooks ()
  (remove-hook 'helm-after-update-hook #'helm-browse-move-to-first-cand-after-point)
  (remove-hook 'helm-move-selection-after-hook #'helm-browse-after-selection-move))

(iter-defun helm-browse-hunk-gen (beg end fun-or-regexp)
  "Return a generator for hunks matching text according to FUN-OR-REGEXP.
The search will be bound to the region of text between BEG and
END.  FUN-OR-REGEXP is either a regexp or a function.

Whenever this generator is be called, it will first move point
right after the upper bound from the last found hunk.  If
FUN-OR-REGEXP is a function, calling it must either return a
list (start end) indicating the bounds of the next hunk, or nil
when there are no more matches."
  (let ((searcher (cond ((stringp fun-or-regexp)
                         (lambda () (and (helm-browse--search-forward-regexp fun-or-regexp end t)
                                    (list (match-beginning 0) (match-end 0)))))
                        ((functionp fun-or-regexp) fun-or-regexp)
                        (t (error "Bad argument"))))
        (opoint (point))   (done nil)
        (current-bounds (list (1- beg) (1- beg)))   new-bounds)
    (while (not done)
      (let ((inhibit-quit t)) ;protect block from unwinding
        (goto-char (1+ (nth 1 current-bounds)))
        (setq new-bounds (funcall searcher))
        (cond
         ((or (not new-bounds) (>  (nth 1 current-bounds)    end))
          (setq done t))
         ((<= (nth 0 new-bounds) (nth 1 current-bounds))
          (message "Error: search didn't lead us forward")
          (sit-for 2)
          (setq done t))))
      (goto-char opoint)
      (unless done (iter-yield (setq current-bounds new-bounds))))))

(defvar helm-browse-parsed-pattern nil)

(defun helm-browse-parse-pattern (pattern &optional flags)
  ;;$$$$$$ Should "!" and any flag be interchanged, like it is in `helm-buffers'?
  ;;$$$$$$ currently, no one flag must be prefix of another one
  '(Examples:

    (helm-browse-parse-pattern "a -bfoo !-cbar")
    ==>
    ((nil nil "a")
     (nil nil "-bfoo")
     (nil t "-cbar"))

    (helm-browse-parse-pattern "a -bfoo !-cbar" '("-a" "-b"))
    ==>
    ((nil nil "a")
     ("-b" nil "foo")
     (nil t "-cbar"))
    )

  (let ((subpatterns (split-string
                      (replace-regexp-in-string helm-mm-space-regexp "\000\000" pattern)
                      " " t)))
    (setq helm-browse-parsed-pattern
          (mapcar
           (lambda (subpattern)
             (let ((negated nil) current-flag)
               (when (string-match "\\`!\\(.*\\)\\'" subpattern)
                 (setq negated t
                       subpattern (match-string 1 subpattern)))
               (when (setq current-flag
                           (cl-some
                            (lambda (flag)
                              (if (string-match (concat "\\`" (regexp-quote flag) "\\(.*\\)")
                                                subpattern)
                                  flag
                                nil))
                            flags))
                 (setq subpattern (match-string 1 subpattern)))
               (list current-flag negated (replace-regexp-in-string "\000\000" " " subpattern))))
           subpatterns))))

(defun helm-browse-init-cache (domain-definer cache-var)
  (with-helm-current-buffer
    (set cache-var
         (iterator-to-ilist
          (helm-browse-hunk-gen (point-min) (point-max) domain-definer)))))

(defvar helm-browse-default-matcher
  (lambda (bounds)
    (let ((string (apply #'buffer-substring bounds)))
      (cl-every (lambda (descr)
                  (funcall (if (nth 1 descr) #'not #'identity)
                           (let ((case-fold-search
                                  (let ((case-fold-search nil))
                                    (pcase helm-case-fold-search
                                      (`smart (not (string-match-p "[[:upper:]]" (nth 2 descr))))
                                      (_      case-fold-search)))))
                             (string-match-p (nth 2 descr) string))))
                (helm-browse-parse-pattern helm-pattern '())))))

(defun helm-browse-make-cand-lister (cache-var &optional matcher)
  (cl-callf or matcher helm-browse-default-matcher)
  (lambda ()
    (let* ((mb (window-buffer (active-minibuffer-window)))
           (ol (with-current-buffer mb (make-overlay (point-max) (point-max) nil t t)))
           (nb-matches -1)
           (report-progress
            (lambda ()
              (ignore-errors
                (let ((msg (format "  [%d]" (cl-incf nb-matches))))
                  (overlay-put ol 'after-string msg)
                  (put-text-property 0 1 'cursor t msg)
                  (sit-for 0))))))
      (funcall report-progress)
      (unwind-protect (with-helm-current-buffer
                        (save-excursion
                          (iterator-to-list
                           (iterator-map
                            (lambda (range)
                              (funcall report-progress)
                              (concat
                               (format "%s-%s:: " (car range) (cadr range))
                               (apply #'buffer-substring-no-properties range)))
                            (iterator-take
                             (helm-candidate-number-limit (helm-get-current-source))
                             (iterator-filter
                              matcher
                              (ilist-to-iterator (symbol-value cache-var))))))))
        (delete-overlay ol)))))

(defvar helm-browse-match-region-overlay nil)
(defvar helm-browse-highlight-match-overlays '())

(defvar helm-browse-highlight-cands-overlays '())

(defun helm-browse-highlight-range (start end)
  (if (not helm-browse-match-region-overlay)
      (setq helm-browse-match-region-overlay (funcall #'make-overlay start end))
    (funcall 'move-overlay helm-browse-match-region-overlay start end))
  (overlay-put helm-browse-match-region-overlay 'face 'helm-browse-this-cand)
  (overlay-put helm-browse-match-region-overlay 'priority 100)
  (overlay-put
   helm-browse-match-region-overlay
   'before-string
   (propertize (purecopy " ")
               'display  (list 'left-fringe 'right-triangle))))

(defun helm-browse-highlight-matches (regexp)
  
  (when (window-live-p (helm-window))
    (while-no-input
      (let* ((win   (get-buffer-window helm-current-buffer))
             (start (window-start win))
             (end   (save-excursion
                      (goto-char start)
                      (forward-line (1+ (window-height win)))
                      (point))))

        ;; Candidate regions

        (when helm-browse-highlight-cands-overlays
          (mapc #'delete-overlay helm-browse-highlight-cands-overlays)
          (setq helm-browse-highlight-cands-overlays nil))

        (let ((visible-regions '()))
          (with-helm-buffer
            (let (cand-end (last-cand-end -1))
              (save-excursion
                (while (and (not (bobp))
                            (progn (setq cand-end (get-text-property (point) 'end))
                                   (or (not cand-end) ;separator
                                       (< start cand-end))))
                  (when (and cand-end (not (= cand-end last-cand-end)))
                    (setq last-cand-end cand-end)
                    (push (cons (get-text-property (point) 'begin) cand-end)
                          visible-regions))
                  (backward-line 1))))
            (let (cand-beg  (last-cand-beg -1))
              (save-excursion
                (while (and (not (eobp))
                            (progn (setq cand-beg (get-text-property (point) 'begin))
                                   (or (not cand-beg) ;separator
                                       (< cand-beg end))))
                  (when (and cand-beg (not (= cand-beg last-cand-beg)))
                    (setq last-cand-beg cand-beg)
                    (push (cons cand-beg (get-text-property (point) 'end))
                          visible-regions))
                  (forward-line 1)))))
          (mapc (lambda (region)
                  (let ((ov (make-overlay (car region) (cdr region))))
                    (push ov helm-browse-highlight-cands-overlays)
                    (overlay-put ov 'face 'helm-browse-cand-region)))
                visible-regions)

        
          ;; Regexp matches

          (when helm-browse-highlight-match-overlays
            (mapc #'delete-overlay helm-browse-highlight-match-overlays)
            (setq helm-browse-highlight-match-overlays nil))

          (mapc (lambda (region)
                  (let ((start (car region))
                        (end   (cdr region)))
                    (save-restriction
                      (unless (string= regexp "")
                        (save-match-data
                          (save-excursion
                            (goto-char start)
                            (while (and (<= (point) end) (re-search-forward regexp end t))
                              (if (= (match-beginning 0) (match-end 0))
                                  (goto-char (1+ (match-end 0)))
                                (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                                  (overlay-put overlay 'priority 1000)
                                  (push overlay helm-browse-highlight-match-overlays)
                                  (overlay-put overlay 'face 'helm-browse-highlight-match))
                                (goto-char (match-end 0))))))))))
                visible-regions))))))

(defun helm-browse-ov-cleanup ()
  (run-with-idle-timer
   0.0 nil
   (lambda ()
     (when helm-browse-match-region-overlay
       (delete-overlay helm-browse-match-region-overlay)
       (setq helm-browse-match-region-overlay nil))
     (when helm-browse-highlight-match-overlays
       (mapc #'delete-overlay helm-browse-highlight-match-overlays)
       (setq helm-browse-highlight-match-overlays nil))
     (when helm-browse-highlight-cands-overlays
      (mapc #'delete-overlay helm-browse-highlight-cands-overlays)
      (setq helm-browse-highlight-cands-overlays nil)))))

(add-hook 'helm-cleanup-hook
          (defun helm-browse-cleanup-hook-fun ()
            (run-with-idle-timer 0.0 nil #'helm-browse-ov-cleanup)))

(defvar helm-browse-orig-buffer-pos nil)

(defvar helm-browse-force-highlighting-matches nil)

(defun helm-browse-move-to-first-cand-after-point ()
  (interactive)
  (with-helm-window
    (if (helm-empty-buffer-p)
        (progn
          (when helm-browse-match-region-overlay
            (delete-overlay helm-browse-match-region-overlay))
          (mapc #'delete-overlay helm-browse-highlight-match-overlays)
          (with-helm-current-buffer
            (goto-char helm-browse-orig-buffer-pos)))
      (goto-char (point-min))
      (forward-line 1)
      (let (buffer-pos)
        (while (and (not (eobp))
                    (progn (setq buffer-pos (get-text-property (point) 'end))
                           (or (not buffer-pos) ;separator
                               (< buffer-pos helm-browse-orig-buffer-pos))))
          (forward-line 1))
        (when (eobp) (backward-line 1))
        (forward-line 0) ; Avoid scrolling right on long lines.
        (when (helm-pos-multiline-p)
          (helm-move--beginning-of-multiline-candidate))
        (helm-mark-current-line)
        (let ((helm-browse-force-highlighting-matches t))
          (helm-browse-default-goto (helm-get-selection)))))))

(defun helm-browse-update-function ()
  (helm-browse-move-to-first-cand-after-point))

(defun helm-browse-default-real-to-display (cand)
  (if (not (string-match "\\`\\([0-9-]*:: \\)" cand))
      cand
    (pcase-let ((`(,beg ,end)
                 (save-match-data
                   (mapcar #'string-to-number
                           (split-string (substring (match-string 1 cand) 0 -3) "-")))))
      (propertize
       (replace-match "" nil nil cand 1)
       'begin beg 'end end ))))

(defun helm-browse-default-candidate-transformer (cands)
  (let (beg end (regexps (mapcar
                          (lambda (pattern)
                            (if (string-match "\\`\\*\\(.*\\)\\'" pattern)
                                (regexp-quote (match-string 1 pattern))
                              pattern))
                          (delq nil
                                (mapcar (lambda (subpattern)
                                          (if (and (null (nth 1 subpattern))
                                                   (not (string= "" (nth 2 subpattern))))
                                              (if (null (nth 0 subpattern))
                                                  (nth 2 subpattern)
                                                (regexp-quote (nth 2 subpattern)))
                                            nil))
                                        helm-browse-parsed-pattern)))))
    (if (null regexps)
        cands
      (mapcar (lambda (cand)
                (condition-case nil
                    (let ((match cand))
                      (with-temp-buffer
                        (insert match)
                        (goto-char (point-min))
                        (cl-loop for reg in regexps
                                 do
                                 (while (and (re-search-forward reg nil t)
                                             (> (- (setq end (match-end 0))
                                                   (setq beg (match-beginning 0)))
                                                0))
                                   (add-text-properties beg end '(face helm-match)))
                                 do (goto-char (point-min))) 
                        (buffer-string)))
                  (error nil)))
              cands))))

(defun helm-browse-simple-goto (cand)
  (string-match "^\\([0-9]+\\)-\\([0-9]+\\)::" cand)
  (let  ((start (string-to-number (match-string 1 cand)))
         (end   (string-to-number (match-string 2 cand))))
    (helm-goto-char start)
    (helm-browse-highlight-range start end)))

(defun helm-browse-default-goto (cand)
  (with-helm-current-buffer
    (string-match "^\\([0-9]+\\)-\\([0-9]+\\)::" cand)
    (let*  ((start   (string-to-number (match-string 1 cand)))
            (end     (string-to-number (match-string 2 cand)))
            (regexps (delq nil
                           (mapcar (lambda (subpattern)
                                     (if (and (null (nth 1 subpattern))
                                              (not (string= "" (nth 2 subpattern)))
                                              (null (nth 0 subpattern)))
                                         (nth 2 subpattern)
                                       nil))
                                   helm-browse-parsed-pattern)))
            (regexp-to-highlight (if regexps (mapconcat #'identity regexps "\\|") ".+")))
      (let ((window-scroll-functions
             (cons (lambda (_win &rest _)
                     (with-current-buffer helm-current-buffer
                       (helm-browse-highlight-matches regexp-to-highlight)))
                   window-scroll-functions)))
        (set-window-point (get-buffer-window helm-current-buffer)
                          (goto-char start))
        (redisplay)
        (setq helm-browse-current-pos (point))
        (helm-browse-highlight-range start end)
        (when helm-browse-force-highlighting-matches
          (helm-browse-highlight-matches regexp-to-highlight))
        ;; (when (window-live-p (helm-window))
        ;;   (run-hooks 'post-command-hook))
        ))))

(defun helm-browse-make-source-spec (name domain-definer &optional matcher alist)
  (let* ((cache (make-symbol "cache"))
         (defaults `((name . ,name)
                     (init  (lambda ()
                              (with-helm-current-buffer
                                (setq helm-browse-orig-buffer-pos (point)
                                      helm-browse-buffer-mod-tick (buffer-chars-modified-tick))
                                (helm-browse-init-cache (funcall ',domain-definer) ',cache)
                                (helm-browse-prepair-hooks))))
                     (candidates . ,(helm-browse-make-cand-lister cache matcher))
                     (candidate-number-limit . 2500)
                     (volatile) (match . identity) (nohighlight) (requires-pattern . 3)
                     (no-matchplugin) (real-to-display . helm-browse-default-real-to-display)
                     (candidate-transformer . helm-browse-default-candidate-transformer)
                     (action . (("Go there" . helm-browse-default-goto)))
                     (resume . helm-browse-resume)
                     (cleanup . helm-browse-restore-hooks))))
    (cl-callf reverse defaults)
    (dolist (default defaults)
      (unless (assoc (car default) alist)
        (push default alist)))
    alist))

(defvar helm-browse-buffer-mod-tick nil)

(defun helm-browse-resume ()
  (with-helm-buffer
    ;; (mapc 'helm-force-update--reinit (helm-get-sources))
    (let ((source helm-current-source))
      (helm-funcall-with-source source (assoc-default 'init source)))
    (run-with-idle-timer 0 nil #'helm-update)))

(defvar helm-browse-current-pos 1)

(defun helm-browse-goto-next-match ()
  (interactive)
  (helm-browse-default-goto (helm-get-selection)))

(defvar helm-browse-default-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map [(control ?s)] #'helm-browse-goto-next-match)
    map))

;; User stuff

(defun helm-browse-make-outline-source ()
  (helm-browse-make-source-spec
   "Browse headlines"
   (lambda () (concat outline-regexp ".*$"))
   nil
   '((multiline))))

(defun helm-browse-outline ()
  (interactive)
  (helm :sources (helm-browse-make-outline-source) :buffer "*helm browse outline*"))


;; text properties

(defun helm-browse-text-prop-definer (prop check find-next)
  (lambda () (let ((beg (if (funcall check (point) prop)
                       (point)
                     (funcall find-next (point) prop))))
          (when (and beg (= beg (point-max)))
            (setq beg nil))
          (and beg (list beg (funcall find-next beg prop))))))

(defun helm-browse-text-property-source (prop)
  (helm-browse-make-source-spec
   (format "Browse %s" prop)
   (lambda () (helm-browse-text-prop-definer prop #'get-text-property #'next-single-property-change))
   nil
   `((multiline)
     (keymap . ,helm-browse-default-map))))

(defun helm-browse-char-property-source (prop)
  (helm-browse-make-source-spec
   (format "Browse %s" prop)
   (lambda () (helm-browse-text-prop-definer prop #'get-char-property #'next-single-char-property-change))
   nil
   `((multiline)
     (keymap . ,helm-browse-default-map))))

(defun helm-browse-diff ()
  (interactive)
  (require 'diff-hl)
  (helm :sources (helm-browse-char-property-source 'diff-hl-hunk)))


;; Dired

(defun helm-browse-search-forward-dired (regexp)
  (let ((match nil))
    (while (and (not match) (helm-browse--search-forward-regexp regexp nil t))
      (let ((file-beg (save-excursion (and (dired-move-to-filename)
                                           (not (looking-at "\\.\\.?/?$"))
                                           (point)))))
        (cond
         ((not file-beg)
          (forward-line))
         ((< (match-beginning 0) file-beg)
          (goto-char file-beg))
         (t (setq match (list file-beg (line-end-position)))))))
    match))

(defun helm-browse-dired-make-gen ()
 (let* ((parsed-pattern (helm-browse-parse-pattern helm-pattern '("-l" "-d" "-a" "-m" "-r"
                                                                  "-s0" "-s>" "-s<"
                                                                  "-h=" "-h>" "-h<")))
        (regexp (mapconcat
                 #'identity
                 (or
                  (mapcar
                   (lambda (pattern)
                     (thread-last pattern
                       (replace-regexp-in-string "^\\^" "")
                       (replace-regexp-in-string "\\$$" "")))
                   (mapcar #'cl-caddr (seq-filter (lambda (entry) (not (or (car entry) (cadr entry))))
                                               parsed-pattern)))
                  '(".+"))
                 "\\|"))
        (searcher (lambda () (helm-browse-search-forward-dired regexp))))
   (helm-browse-hunk-gen (point-min) (point-max) searcher)))

(defun helm-browse-dired-get-cands ()
  (with-helm-current-buffer
    (iterator-to-list
     (iterator-map
      (lambda (range)
        (concat
         (format "%s-%s:: " (car range) (cadr range))
         (apply #'buffer-substring-no-properties range)))
      (iterator-take
       (helm-candidate-number-limit (helm-get-current-source))
       (iterator-filter
        helm-browse-dired-matcher
        (helm-browse-dired-make-gen)))))))

(defvar helm-browse-dired-matcher
  (lambda (bounds)
    (save-excursion
      (goto-char (car bounds))
      (dired-move-to-filename)
      (let* ((file-in-buffer (dired-get-filename 'verbatim t))
             (file-abs       (dired-get-filename nil       t))
             (symlink?       (helm-browse--delay-expression (file-symlink-p   file-abs)))
             (directory?     (helm-browse--delay-expression (file-directory-p file-abs)))
             (regular?       (helm-browse--delay-expression (file-regular-p   file-abs)))
             (attributes     (helm-browse--delay-expression (file-attributes  file-abs)))
             (links          (helm-browse--delay-expression (file-nlinks      file-abs)))
             (size           (helm-browse--delay-expression (nth 7 (funcall attributes))))
             (get-size    (lambda (s)
                               (let ((suffix nil))
                                 (when (string-match "\\`\\(.*\\)\\([kKmMgGtT]\\)\\'" s)
                                   (setq suffix (downcase (match-string 2 s))
                                         s      (match-string 1 s)))
                                 (* (if (and suffix (string= s ""))
                                        1.0
                                      (string-to-number s))
                                    (pcase suffix
                                      (`nil  1.0)
                                      (`"k"  1000.0)
                                      (`"m"  1000000.0)
                                      (`"g"  1000000000.0)
                                      (`"t"  1000000000000.0)))))))
        (cl-every (lambda (descr)
                    (funcall (if (nth 1 descr) #'not #'identity)
                             (let ((case-fold-search
                                    (let ((case-fold-search nil))
                                      (pcase helm-case-fold-search
                                        (`smart (not (string-match-p "[[:upper:]]" (nth 2 descr))))
                                        (_      case-fold-search)))))
                               (pcase (nth 0 descr)
                                 (`nil  (string-match-p (nth 2 descr)  file-in-buffer))
                                 (`"-a" (string-match-p (nth 2 descr)  file-abs))
                                 (`"-l" (let ((choice (nth 2 descr)))
                                          (if (string= choice "")
                                              (funcall symlink?)
                                            (and (funcall symlink?)
                                                 (string-match-p choice (file-truename file-abs))))))
                                 (`"-d" (funcall directory?))
                                 (`"-r" (funcall regular?))
                                 (`"-m" (string-match-p
                                         (let ((choice (nth 2 descr)))
                                           (if (string= choice "") "[^ ]" (regexp-quote choice)))
                                         (buffer-substring (line-beginning-position)
                                                           (1+ (line-beginning-position)))))
                                 (`"-s0" (= (funcall size) 0))
                                 (`"-s>" (> (funcall size) (funcall get-size (nth 2 descr))))
                                 (`"-s<" (< (funcall size) (funcall get-size (nth 2 descr))))
                                 (`"-h="   (=  (funcall links) (string-to-number (nth 2 descr))))
                                 (`"-h>"  (>  (funcall links) (string-to-number (nth 2 descr))))
                                 (`"-h<"  (<  (funcall links) (string-to-number (nth 2 descr))))))))
                  (let ((parsed-pattern (helm-browse-parse-pattern helm-pattern
                                                                   '("-l" "-d" "-a" "-m" "-r"
                                                                     "-s0" "-s>" "-s<"
                                                                     "-h=" "-h>" "-h<"))))
                    ;; $$$$$$FIXME avoid error when jumping to buffer pos
                    (unless (cl-some (lambda (entry) (and (not (car entry))
                                                     (not (cadr entry))))
                                     parsed-pattern)
                      (push (list nil nil ".+") parsed-pattern))
                    parsed-pattern))))))

(defun helm-browse-dired-mark (_cand)
  (save-excursion
    (let ((dired-marker-char (if (not helm-current-prefix-arg)
                                    dired-marker-char
                                  (message "Mark with character: ")
                                  (read-char))))
      (mapc
       (lambda (cand)
         (goto-char (progn
                      (string-match "^\\([0-9]+\\)" cand)
                      (string-to-number (match-string 1 cand))))
         (dired-mark nil))
       (helm-marked-candidates)))))

(defun helm-browse-dired-unmark (_cand)
  (save-excursion
    (mapc
     (lambda (cand)
       (goto-char (progn
                    (string-match "^\\([0-9]+\\)" cand)
                    (string-to-number (match-string 1 cand))))
       (dired-unmark nil))
     (helm-marked-candidates))))

(defun helm-browse-dired-unmark-all-in-buffer (&optional _cand)
  (interactive)
  (with-helm-current-buffer (dired-unmark-all-marks)))

(defvar helm-browse-dired-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-browse-default-map)
    (define-key map [(meta shift ?u)] #'helm-browse-dired-unmark-all-in-buffer)
    map))

(defvar helm-source-browse-dired
  `((name . "Browse Dired")
    (init  (lambda ()
             (with-helm-current-buffer
               (setq helm-browse-orig-buffer-pos (point)
                     helm-browse-buffer-mod-tick (buffer-chars-modified-tick)
                     helm-browse-current-pos nil)
               (helm-browse-prepair-hooks))))
    (candidates . helm-browse-dired-get-cands)
    (candidate-number-limit . 2500)
    (update . helm-browse-update-function)
    (volatile) (match . identity) (nohighlight) (requires-pattern . 2)
    (no-matchplugin)
    (real-to-display . helm-browse-default-real-to-display)
    (candidate-transformer . helm-browse-default-candidate-transformer)
    (resume . helm-browse-resume)
    (action . (("Go there"                          . helm-browse-default-goto)
               ("Mark (prefix: choose marker char)" . helm-browse-dired-mark)
               ("Unmark"                            . helm-browse-dired-unmark)))
    (keymap . ,helm-browse-dired-map)
    (history . helm-browse-dired-history)
    (cleanup . helm-browse-restore-hooks))
  "Browse dired.")

(defun helm-browse-dired ()
  (interactive)
  (helm :sources helm-source-browse-dired :buffer "*helm browse dired*"))


;; w3m, eww

(defvar helm-source-w3m-links
  (helm-browse-make-source-spec
   "Browse w3m links"
   (lambda () (lambda () (if (not (w3m-goto-next-anchor))
                             nil
                           (list (point) (next-single-property-change
                                          (point) 'w3m-anchor-sequence)))))
   nil '((history . helm-browse-w3m-history))))

(defvar helm-source-eww-links
  (helm-browse-make-source-spec
   "Browse eww links"
   (lambda () (lambda () (let ((skip (text-property-any (point) (point-max) 'help-echo nil)))
                 (if (not (setq skip (text-property-not-all skip (point-max)
                                                            'help-echo nil)))
                     nil
                   (goto-char skip)
                   (list (point) (next-property-change (point)))))))
   nil
   '((history . helm-browse-eww-history))))

(defun helm-browse-w3m-links ()
  (interactive)
  (helm :sources helm-source-w3m-links :buffer "*helm browse w3m*"))

(defun helm-browse-eww-links ()
  (interactive)
  (helm :sources helm-source-eww-links :buffer "*helm browse eww*"))


;; comint

(defvar helm-source-browse-comint-inputs
  (helm-browse-make-source-spec
   "Browse prompts"
   (lambda () (let ((input-line-rx (concat comint-prompt-regexp "\\(.*\\)")))
           (lambda () (and (search-forward-regexp input-line-rx nil t)
                      (list (match-beginning 1) (match-end 1))))))
   nil
   '((history . helm-browse-comint-history))))

(defun helm-browse-comint-inputs ()
  (interactive)
  (helm :sources helm-source-browse-comint-inputs :buffer "*helm browse comint*"))


;; Proced: use proced-pid text property, und schlage in
;; proced-process-alist nach.

(eval-when-compile (require 'proced nil t))

(defvar helm-browse-proced-matcher
  (lambda (bounds)
    (save-excursion
      (goto-char (car bounds))
      (proced-move-to-goal-column)
      (let* ((process-name (buffer-substring-no-properties (point) (line-end-position))))
        (cl-every (lambda (descr)
                    (funcall (if (nth 1 descr) #'not #'identity)
                             (let ((case-fold-search
                                    (let ((case-fold-search nil))
                                      (pcase helm-case-fold-search
                                        (`smart (not (string-match-p "[[:upper:]]" (nth 2 descr))))
                                        (_      case-fold-search)))))
                               (pcase (nth 0 descr)
                                 (`nil  (string-match-p (nth 2 descr)  process-name))
                                 (`"-m" (string-match-p
                                         "[^ ]"
                                         (buffer-substring (car bounds) (1+ (car bounds)))))))))
                  (helm-browse-parse-pattern helm-pattern '("-m")))))))

(defun helm-browse-proced-mark (_cand)
  (save-excursion
    (mapc
     (lambda (cand)
       (goto-char (progn
                    (string-match "^\\([0-9]+\\)" cand)
                    (string-to-number (match-string 1 cand))))
       (proced-mark nil))
     (helm-marked-candidates))))

(defun helm-browse-proced-unmark (_cand)
  (save-excursion
    (mapc
     (lambda (cand)
       (goto-char (progn
                    (string-match "^\\([0-9]+\\)" cand)
                    (string-to-number (match-string 1 cand))))
       (proced-unmark nil))
     (helm-marked-candidates))))

(defvar helm-source-browse-proced
  (helm-browse-make-source-spec
   "Browse Proced"
   (lambda () "^.+$")
   helm-browse-proced-matcher
   '((action . (("Go there" . helm-browse-simple-goto)
                ("Mark"     . helm-browse-proced-mark)
                ("Unmark"   . helm-browse-proced-unmark)))
     (history . helm-browse-proced-history))))

(defun helm-browse-proced ()
  (interactive)
  (helm :sources helm-source-browse-proced :buffer "*helm browse proced*"))


(defvar helm-browse-lines-cand-lister
  (lambda ()
    (with-helm-current-buffer
      (save-excursion
        (iterator-to-list
         (iterator-map
          (lambda (range)
            (concat
             (format "%s-%s:: " (car range) (cadr range))
             (apply #'buffer-substring-no-properties range)))
          (iterator-take
           (helm-candidate-number-limit (helm-get-current-source))
           (helm-browse-lines-make-gen
            (point-min) (point-max)
            helm-pattern)))))))
  "Candidate lister for `helm-source-browse-lines'.")

(defun helm-browse-lines-make-search-fun (include-patterns exclude-patterns get-upper-bound)
  "Return a search function according to the arguments.
Used as third argument of `helm-browse-hunk-gen' in
`helm-browse-lines-make-gen'."
  (let ((any-include-pattern (mapconcat #'identity include-patterns "\\|"))
        (any-exclude-pattern (and exclude-patterns
                                  (mapconcat #'identity exclude-patterns "\\|"))))
    (lambda ()
      (let (done lower-bound upper-bound)
        (while (and (not done) (helm-browse--search-forward-regexp any-include-pattern nil t))
          (goto-char (match-beginning 0))
          (beginning-of-line)
          (setq lower-bound (point))
          (setq upper-bound (funcall get-upper-bound))
          (if (and (cl-every (lambda (regexp)
                               (goto-char lower-bound)
                               (helm-browse--search-forward-regexp regexp upper-bound t))
                             include-patterns)
                   (not (and any-exclude-pattern
                             (progn (goto-char lower-bound)
                                    (helm-browse--search-forward-regexp
                                     any-exclude-pattern upper-bound t)))))
              (progn
                (goto-char upper-bound)
                (search-backward-regexp any-include-pattern)
                (setq done (list lower-bound (line-end-position))))
            (goto-char lower-bound)
            (forward-line)))
        done))))

(defun helm-browse-lines-make-gen (beg end pattern)
  "Return a browse-lines generator for PATTERN."
  ;; (helm-browse-parse-pattern pattern '())
  (let* ((case-fold-search
          (pcase helm-case-fold-search
            (`smart (not (let ((case-fold-search nil)) (string-match-p "[[:upper:]]" helm-pattern))))
            (_      case-fold-search)))
         (include-patterns '()) (exclude-patterns '())
         (get-upper-bound (lambda () (line-end-position +1))))
    (mapc
     (lambda (entry)
       (pcase entry
         (`(nil  nil ,regexp) (push regexp include-patterns))
         (`(nil  t   ,regexp) (push regexp exclude-patterns))
         (`("-n" ,_   "")
          (setq get-upper-bound
                (lambda () (save-excursion
                        (let* ((non-whitespace-line-regexp '(and (* nonl) (+ (not (any space "\n"))) (* nonl)))
                               (regexp-until-par-end (rx-to-string `(and (* (and ,non-whitespace-line-regexp "\n"))
                                                                         ,non-whitespace-line-regexp))))
                          (if (search-forward-regexp regexp-until-par-end nil t)
                              (match-end 0)
                            (point-max)))))))
         (`("-n" ,_   ,n)
          (setq get-upper-bound (lambda () (line-end-position (string-to-number n)))))))
     (helm-browse-parse-pattern pattern '("-n")))
    (helm-browse-hunk-gen
     beg end
     (helm-browse-lines-make-search-fun include-patterns exclude-patterns get-upper-bound))))

(defvar helm-source-browse-lines
  `((name . "Browse text")
    (init  (lambda ()
             (with-helm-current-buffer
               (setq helm-browse-orig-buffer-pos (point)
                     helm-browse-buffer-mod-tick (buffer-chars-modified-tick)
                     helm-browse-current-pos nil)
               (helm-browse-prepair-hooks))))
    (candidates . ,helm-browse-lines-cand-lister)
    (candidate-number-limit . 2500)
    (update . helm-browse-update-function)
    (volatile) (match . identity) (nohighlight) (requires-pattern . 3)
    (no-matchplugin) (multiline)
    (real-to-display . helm-browse-default-real-to-display)
    (candidate-transformer . helm-browse-default-candidate-transformer)
    (action . (("Go there" . helm-browse-default-goto)))
    (resume . helm-browse-resume)
    (keymap . ,helm-browse-default-map)
    (history . helm-browse-history)
    (cleanup . helm-browse-restore-hooks)))

(defun helm-browse-lines ()
  (interactive)
  (helm :sources helm-source-browse-lines :buffer "*helm browse*"))

(defun helm-browse-buffer-from-isearch ()
  "Invoke `helm-browse-lines' from isearch."
  (interactive)
  (let ((input (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (helm :sources helm-source-browse-lines :input input :buffer "*helm browse*")))

(provide 'helm-browse)


;;;; * File Local Vars
;;; Local variables: ***
;;; byte-compile-warnings: (not noruntime) ***
;;; End: ***

;;; helm-browse.el ends here
