;;; electric-case.el --- insert camelCase, snake_case words without "Shift"ing

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Version: 2.2.2
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; 1. Usage

;; 1.A. Overview
;;
;; For example, to try electric-case-mode in java-mode, put following expression
;; into your init file.
;;
;;   (require 'electric-case)
;;
;;   (eval-after-load "cc-mode"
;;     (add-hook 'java-mode-hook electric-case-java-init))
;;
;; Now, when you type following expression as usual in java-mode,
;;
;;   public class test-class{
;;       public void test-method(void){
;;
;; electric-case will automatically convert it into :
;;
;;   public class TestClass{
;;       public void testMethod(void){

;; Settings for some other languages are also available by default. Try:
;;
;;   (eval-after-load "cc-mode"
;;     (add-hook 'c-mode-hook electric-case-c-init))
;;
;;   (eval-after-load "ahk-mode"
;;     (add-hook 'ahk-mode-hook electric-case-ahk-init))
;;
;;   (eval-after-load "scala-mode"
;;     (add-hook 'scala-mode-hook electric-case-scala-init))
;;
;; If you want to use electric-case-mode on other languages than above,
;; you may make your own setting. Read section 2.

;; 1.B. "convert-calls"
;;
;; electric-case do not convert other expressions than declarations, by default. To
;; enable conversion for other expressions, set "electric-case-convert-calls" non-nil.
;;
;;   (setq electric-case-convert-calls t)
;;
;; This sometimes produces confusing results for novice users. For example,
;;
;;   foo-bar
;;
;; is not treated as "foo minus bar", but converted to
;;
;;   fooBar
;;
;; To make "-" treated as subtraction or negation, insert whitespace around it.
;;
;;   foo - bar
;;
;; I recommend to keep "electric-case-convert-calls" nil, because convert-calls may be
;; too noisy. Once declared, symbols may be inserted easily using completion. This
;; script is useful when you TYPE camel-case or snake-case symbols. But in case you do
;; not need to type, not to type is much better.

;; 1.C. "convert-nums", "convert-beginning", and "convert-end"
;;
;; Even if "electric-case-convert-calls" is non-nil, numbers, and hyphens at beginning/end of
;; symbols are not converted.
;;
;;   -foo-1  =>  -foo-1
;;
;; You may change this behavior by turning some of three variables to non-nil.
;;
;;   (setq electric-case-convert-nums t)      hyphens around numbers
;;   (setq electric-case-convert-beginning t) hyphens at beginning of symbols
;;   (setq electric-case-convert-end t)       hyphens at end of symbols
;;
;; When you insert an expression "-foo--1--bar-",
;;
;;                   +---num
;;                   |     +--- num
;;                   V     V
;;    -  f  o  o  -  -  1  -  -  b  a  r  -
;;    ^           ^           ^           ^
;;    |           +--- end    |           +--- end
;;    +--- beginning          +--- beginning
;;
;; electric-case will convert it like:
;;
;;                      num beg end
;;
;;     -foo--1--bar-    nil nil nil
;;     -foo-1--bar      nil nil  t
;;     Foo--1-Bar-      nil  t  nil
;;     -foo1Bar-         t  nil nil
;;     Foo1Bar           t   t   t

;; 1.D. overlays
;;
;; Symbols that may be converted are printed in gray. If this is not confortable for you,
;; evaluate following expression to disable it.
;;
;;   (setq electric-case-pending-overlay nil)
;;
;; Or you may also choose another face for overlay.
;;
;;   (setq electric-case-pending-overlay 'highlight)

;; 1.E. disable electric-case
;;
;; If you want to disable electric-case temporally, use command "M-x electric-case-mode"
;; or evaluate expression below :
;;
;;   (electric-case-mode -1)
;;
;; To activate again, call the same command again, or evaluate expression below :
;;
;;   (electric-case-mode 1)

;; 2. Language Configuration

;; There are two important buffer-local variables. To add settings for other languages,
;; configure them.

;; - electric-case-criteria
;;
;;   Set a function that defines which case to convert the symbol into. The function
;;   will be given 2 arguments: the beginning and the end point of the symbol that is
;;   going to be converted. The function must return one of 'camel, 'ucamel, 'snake,
;;   'usnake, or nil. When the return value is nil, conversion for the symbol is canceled.
;;
;;   Remember, that if "electric-case-convert-calls" is nil, symbols not in declarations are
;;   not expected to be converted. electric-case does not judge if the symbol is in a
;;   declaration. So criteria function should return nil in that case.
;;
;;   Here is an example:
;;
;;     (setq electric-case-criteria
;;           (lambda (b e n)
;;             (let ((proper (text-properties-at b)))
;;               (cond ((member 'font-lock-function-name-face proper) 'snake)
;;                     ((member 'font-lock-variable-name-face proper)
;;                      (if (member '(cpp-macro) (c-guess-basic-syntax))
;;                          'usnake 'snake))
;;                     (electric-case-convert-calls 'snake)
;;                     (t nil)))))
;;
;;   with criteria above, function declarations and variable declarations are converted
;;   into snake_case. Macro declarations are converted into UP_SNAKE_CASE. Other expressions
;;   are converted into snake_case, if "electric-case-convert-calls" is non-nil. Otherwise,
;;   are not converted, even if that contain "-". This may be one of the minimal criterias
;;   for C-language.

;; - electric-case-max-iteration
;;
;;   For example, in Java, the syntactical category of the symbol "what-is-this" below
;;   is not decidable.
;;
;;     what-is-this
;;
;;   But when "symbol;" is added, now "what-is-this" is a name of a class.
;;
;;     what-is-this symbol;
;;
;;   So electric-case can convert it.
;;
;;     WhatIsThis symbol;
;;
;;   In the example above, the symbol "what-is-this" must be checked twice. Then,
;;   "electric-case-max-iteration" must be 2 or greater. Otherwise, "what-is-this" is
;;   not checked twice, and not be converted.

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 fixed java settings
;; 1.0.2 minor fixes
;; 1.0.3 fixed java settings
;; 1.0.4 fixed java settings
;; 1.0.5 fixed C settings
;; 1.1.0 added electric-case-convert-calls
;; 1.1.1 modified arguments for criteria function
;; 1.1.2 added ahk-mode settings
;; 1.1.3 added scala-mode settings, and refactord
;; 1.1.4 fixes and improvements
;; 2.0.0 added pending-overlays
;; 2.0.1 added electric-case-trigger to post-command-hook
;;       deleted variable "convert-calls"
;; 2.0.2 minow fixes for criterias
;; 2.0.3 removed electric-case-trigger from post-command-hook
;; 2.0.4 fixed trigger and added hook again
;; 2.1.0 added 2 custom variables, minor fixes
;; 2.1.1 added 2 custom variables
;; 2.2.0 changed behavior
;;       now only symbols overlayd are converted
;; 2.2.1 fixed bug that words without overlay may converted
;; 2.2.2 fixed bug that electric-case-convert-end is ignored

;;; Code:

;; * constants

(defconst electric-case-version "2.2.2")

;; * mode variables

(defvar electric-case-mode nil)
(make-variable-buffer-local 'electric-case-mode)

(when (not (assq 'electric-case-mode minor-mode-alist))
  (add-to-list 'minor-mode-alist
               '(electric-case-mode " Case")))

(defun electric-case-mode (&optional arg)
  "Toggle electric-case-mode"
  (interactive)
  (setq electric-case-mode (cond ((null arg) (not electric-case-mode))
                                 ((> arg 0) t)
                                 (t nil))))

;; * custom variables

(defvar electric-case-pending-overlay 'shadow)

(defvar electric-case-convert-calls nil)
(defvar electric-case-convert-nums nil)
(defvar electric-case-convert-beginning nil)
(defvar electric-case-convert-end nil)

(defvar electric-case-criteria (lambda (b e) 'camel))
(make-variable-buffer-local 'electric-case-criteria)

(defvar electric-case-max-iteration 2)
(make-variable-buffer-local 'electric-case-max-iteration)

;; * utilities

;; motion

(defun electric-case-backward-symbol (&optional n)
  "an-electric-case-pending-word;|   =>   |an-electric-case-pending-word;"
  (interactive)
  (setq n (or n 1))
  (while (>= (setq n (1- n)) 0)
    (when (= (point) (point-min)) (error "beginning of buffer"))
    (backward-word)
    (if electric-case-convert-nums
        (skip-chars-backward "[:alnum:]-")
      (skip-chars-backward "[:alpha:]-")
      (when (= (char-after) ?-) (forward-char)))
    (unless electric-case-convert-beginning
      (skip-chars-forward "-"))))

(defun electric-case--range (n)
  (save-excursion
    (let* ((pos (point))
           (beg (condition-case err
                    (progn (electric-case-backward-symbol n) (point))
                  (error nil)))
           (end (when beg
                  (goto-char beg)
                  (if electric-case-convert-nums
                      (skip-chars-forward "[:alnum:]-")
                    (skip-chars-forward "[:alpha:]-"))
                  (unless electric-case-convert-end
                    (skip-chars-backward "-"))
                  (point))))
      ;; inside-lo|ng-symbol  =>  nil
      ;; b        p        e
      (if (and end (<= end pos))
          (cons beg end)
        nil))))

;; replace buffer

(defun electric-case--replace-buffer (beg end str)
  "(replace 1 2 \"aa\")
buffer-string   =>   aaffer-string"
  (when (not (string= (buffer-substring-no-properties beg end) str))
    (let ((pos (point))
          (oldlen (- end beg))
          (newlen (length str)))
      (kill-region beg end)
      (goto-char beg)
      (insert str)
      (remove-overlays beg (+ beg newlen))
      (goto-char (+ pos (- newlen oldlen))))))

;; overlay management

(defvar electric-case--overlays nil)
(make-variable-buffer-local 'electric-case--overlays)

(defun electric-case--put-overlay (n)
  (let ((range (electric-case--range n)))
    (when range
      (let ((ov (make-overlay (car range) (cdr range))))
        (overlay-put ov 'face electric-case-pending-overlay)
        (add-to-list 'electric-case--overlays ov)))))

(defun electric-case--remove-overlays ()
  (interactive)
  (mapc 'delete-overlay electric-case--overlays)
  (setq electric-case--overlays nil))

(defun electric-case--not-on-overlay-p ()
  (let ((res t) (pos (point)))
    (dolist (ov electric-case--overlays res)
      (setq res (and res
                     (or (< pos (overlay-start ov))
                         (< (overlay-end ov) pos)))))))

;; * commands

(defun electric-case--convert-all ()
  (dolist (ov electric-case--overlays)
    (let ((beg (overlay-start ov))
          (end (overlay-end ov)))
      ;; vvv i dont remember why i added whis line vvv
      (when (string-match "[a-z]" (buffer-substring-no-properties beg end))
        (let* ((type (apply electric-case-criteria (list beg end)))
               (str (buffer-substring-no-properties beg end))
               (wlst (split-string str "-"))
               (convstr (case type
                          ('ucamel (mapconcat '(lambda (w) (upcase-initials w)) wlst ""))
                          ('camel (concat
                                   (car wlst)
                                   (mapconcat '(lambda (w) (upcase-initials w)) (cdr wlst) "")))
                          ('usnake (mapconcat '(lambda (w) (upcase w)) wlst "_"))
                          ('snake (mapconcat 'identity wlst "_"))
                          (t nil))))
          (when convstr
            (electric-case--replace-buffer beg end convstr))))))
  (electric-case--remove-overlays))

(defun electric-case--post-command-function ()
  (when electric-case-mode
    ;; update overlay
    (when (and (eq 'self-insert-command (key-binding (this-single-command-keys)))
               (characterp last-command-event)
               (string-match
                (if electric-case-convert-nums "[a-zA-Z0-9]" "[a-zA-Z]")
                (char-to-string last-command-event)))
      (electric-case--remove-overlays)
      (let (n)
        (dotimes (n electric-case-max-iteration)
          (electric-case--put-overlay (- electric-case-max-iteration n)))))
    ;; electric-case trigger
    (when (and (electric-case--not-on-overlay-p)
               (not mark-active))
      (electric-case--convert-all))))

(add-hook 'post-command-hook 'electric-case--post-command-function)

;; * settings
;; ** utilities

(defun electric-case--possible-properties (beg end)
  (let* ((ret (point))
         (str (buffer-substring beg end))
         (convstr (replace-regexp-in-string "-" "" str))
         (val (progn (electric-case--replace-buffer beg end convstr)
                     (font-lock-fontify-buffer)
                     (sit-for 0)
                     (text-properties-at beg))))
    (electric-case--replace-buffer beg (+ beg (length convstr)) str)
    (font-lock-fontify-buffer)
    val))

(defun electric-case--this-line-string ()
  (buffer-substring (save-excursion (beginning-of-line) (point))
                    (save-excursion (end-of-line) (point))))

;; ** c-mode

(defun electric-case-c-init ()

  (electric-case-mode 1)
  (setq electric-case-max-iteration 2)

  (setq electric-case-criteria
        (lambda (b e)
          (let ((proper (electric-case--possible-properties b e))
                (key (key-description (this-single-command-keys))))
            (cond
             ((member 'font-lock-variable-name-face proper)
              ;; #ifdef A_MACRO  /  int variable_name;
              (if (member '(cpp-macro) (c-guess-basic-syntax)) 'usnake 'snake))
             ((member 'font-lock-string-face proper) nil)
             ((member 'font-lock-comment-face proper) nil)
             ((member 'font-lock-keyword-face proper) nil)
             ((member 'font-lock-function-name-face proper) 'snake)
             ((member 'font-lock-type-face proper) 'snake)
             (electric-case-convert-calls 'snake)
             (t nil)))))

  (defadvice electric-case-trigger (around electric-case-c-try-semi activate)
    (when (and electric-case-mode
               (eq major-mode 'c-mode))
      (if (not (string= (key-description (this-single-command-keys)) ";"))
          ad-do-it
        (insert ";")
        (backward-char)
      ad-do-it
      (delete-char 1))))
  )

;; ** java-mode

(defconst electric-case-java-primitives
  '("boolean" "char" "byte" "short" "int" "long" "float" "double" "void"))

(defun electric-case-java-init ()

  (electric-case-mode 1)
  (setq electric-case-max-iteration 2)

  (setq electric-case-criteria
        (lambda (b e)
          ;; do not convert primitives
          (when (not (member (buffer-substring b e) electric-case-java-primitives))
            (let ((proper (electric-case--possible-properties b e))
                  (str (electric-case--this-line-string)))
              (cond
               ((string-match "^import" str)
                ;; import java.util.ArrayList;
                (if (= (char-before) ?\;) 'ucamel nil))
               ;; annotation
               ((save-excursion (goto-char b)
                                (and (not (= (point) (point-min)))
                                     (= (char-before) ?@)))
                'camel)
               ((member 'font-lock-string-face proper) nil)
               ((member 'font-lock-comment-face proper) nil)
               ((member 'font-lock-keyword-face proper) nil)
               ((member 'font-lock-type-face proper) 'ucamel)
               ((member 'font-lock-function-name-face proper) 'camel)
               ((member 'font-lock-variable-name-face proper) 'camel)
               (electric-case-convert-calls 'camel)
               (t nil))))))

  (defadvice electric-case-trigger (around electric-case-java-try-semi activate)
    (when (and electric-case-mode
               (eq major-mode 'java-mode))
      (if (not (string= (key-description (this-single-command-keys)) ";"))
          ad-do-it
        (insert ";")
        (backward-char)
        ad-do-it
        (delete-char 1))))
  )

;; ** scala-mode

(defun electric-case-scala-init ()

  (electric-case-mode 1)
  (setq electric-case-max-iteration 2)

  (setq electric-case-criteria
        (lambda (b e)
          (when (not (member (buffer-substring b e) electric-case-java-primitives))
            (let ((proper (electric-case--possible-properties b e)))
              (cond
               ((member 'font-lock-string-face proper) nil)
               ((member 'font-lock-comment-face proper) nil)
               ((member 'font-lock-keyword-face proper) nil)
               ((member 'font-lock-type-face proper) 'ucamel)
               ((member 'font-lock-function-name-face proper) 'camel)
               ((member 'font-lock-variable-name-face proper) 'camel)
               (electric-case-convert-calls 'camel)
               (t nil))))))
  )

;; ** ahk-mode

(defun electric-case-ahk-init ()

  (electric-case-mode 1)
  (setq electric-case-max-iteration 1)

  (setq electric-case-criteria
        (lambda (b e)
          (let ((proper (electric-case--possible-properties b e)))
            (cond
             ((member 'font-lock-string-face proper) nil)
             ((member 'font-lock-comment-face proper) nil)
             ((member 'font-lock-keyword-face proper) 'ucamel)
             (electric-case-convert-calls 'camel)
             (t nil)))))
  )

;; * provide

(provide 'electric-case)

;;; electric-case.el ends here
