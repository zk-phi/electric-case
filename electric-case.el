;;; electric-case.el --- Electric case conversion.

;; Copyright (C) 2012 zk_phi

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

;; Version: 1.0.0
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; 1. Usage

;; For example, to try electric-case-mode in java-mode, add following expression
;; into your init file.
;;
;;   (add-hook 'java-mode-hook electric-case-java-init)
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
;;
;; "electric-case-java-init" and "electric-case-c-init" is prepared by default.

;; 2. Adding settings

;; There are three important buffer-local variables. To add settings for other
;; languages, set these variables.

;; * electric-case-mode
;;
;;   When non-nil, electric-case-mode-map is activated
;;
;;     (electric-case-mode 1)

;; * electric-case-mode-map
;;
;;   Bind keys that you want use as electric-case trigger, to "electric-case-trigger"
;;   command. When triggered, the previous symbol before cursor will be converted.
;;
;;     (define-key electric-case-mode-map (kbd "SPC") 'electric-case-trigger)
;;     (define-key electric-case-mode-map (kbd "(") 'electric-case-trigger)
;;     (define-key electric-case-mode-map (kbd ";") 'electric-case-trigger)
;;     (define-key electric-case-mode-map (kbd ",") 'electric-case-trigger)
;;
;;   Note that "electric-case-trigger" command will call original command, after
;;   conversion. For example, even if (kbd "SPC") is bound to electric-case trigger,
;;   still whitespace is inserted with (kbd "SPC").

;; * electric-case-criteria
;;
;;   Set a function that defines which case to convert the symbol into. The function
;;   will be given 2 arguments: beginning point of the symbol, and end point of the
;;   symbol. The function must return 'camel, 'ucamel, 'snake, 'usnake, or nil. When
;;   the return value is nil, the symbol will not be converted.
;;
;;     (setq electric-case-criteria
;;           (lambda (b e)
;;             (let ((proper (text-properties-at b)))
;;               (cond ((member 'font-lock-function-name-face proper) 'snake)
;;                     ((member 'font-lock-variable-name-face proper)
;;                      (if (member '(cpp-macro) (c-guess-basic-syntax))
;;                          'usnake 'snake)
;;                     (t nil)))))
;;
;;   For example, with criteria above, function declarations and variable declarations
;;   are converted into snake_case. Macro declarations are converted into UP_SNAKE_CASE.
;;
;;     int a-variable;  =>  int a_variable;
;;
;;   But other expressions are not converted.
;;
;;     a = b-c;  =>  a = b-c; (NOT "a = bC;")

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 fixed errors in java-init

;;; Code:

;; * constants

(defconst electric-case-version "1.0.1")

;; * variables

(defvar electric-case-mode nil)
(make-variable-buffer-local 'electric-case-mode)

(defvar electric-case-criteria (lambda (b e) 'camel))
(make-variable-buffer-local 'electric-case-criteria)

(defvar electric-case-mode-map (make-sparse-keymap))
(make-variable-buffer-local 'electric-case-mode-map)

(when (not (assq 'electric-case-mode minor-mode-alist))
  (add-to-list
   'minor-mode-alist
   '(electric-case-mode " Case")))

(when (not (assq 'electric-case-mode-map minor-mode-map-alist))
  (add-to-list
   'minor-mode-map-alist
   (cons 'electric-case-mode electric-case-mode-map)))

(defun electric-case-mode (&optional arg)
  "Toggle electric-case-mode"
  (interactive)
  (setq electric-case-mode (cond ((null arg) (not electric-case-mode))
                                 ((> arg 0) t)
                                 (t nil))))

;; * utilities

;; reference | http://www.emacswiki.org/emacs/CamelCase

(defun electric-case-convert (str type)
  (let ((wlst (split-string str "-")))
    (cond ((eq type 'ucamel)
           (mapconcat '(lambda (w) (capitalize w)) wlst ""))
          ((eq type 'camel)
           (concat
            (car wlst)
            (mapconcat '(lambda (w) (capitalize w)) (cdr wlst) "")))
          ((eq type 'snake)
           (mapconcat 'identity wlst "_"))
          ((eq type 'usnake)
           (mapconcat '(lambda (w) (upcase w)) wlst "_"))
          (t
           (mapconcat 'identity wlst "-")))))

(defun electric-case-trigger ()
  (interactive)
  (let* ((beg (save-excursion (+ (point) (skip-chars-backward "[:alnum:]-"))))
         (end (point))
         (str (buffer-substring-no-properties beg end))
         (type (apply electric-case-criteria (list beg end))))
    ;; replace original symbol with converted symbol
    (delete-region beg end)
    (goto-char beg)
    (insert (electric-case-convert str type)))
  ;; call original command
  (when (interactive-p)
    (let ((electric-case-mode nil))
      (call-interactively (key-binding (this-single-command-keys))))))

;; * examples

(defun electric-case-c-init ()

  (electric-case-mode 1)

  (setq electric-case-criteria
        (lambda (b e)
          (let ((proper (text-properties-at b)))
            (cond ((member 'font-lock-function-name-face proper) 'snake)
                  ((member 'font-lock-variable-name-face proper)
                   (if (member '(cpp-macro) (c-guess-basic-syntax))
                       'usnake 'snake))
                  (t nil)))))

  (define-key electric-case-mode-map (kbd "SPC") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd "(") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd ";") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd ",") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd "=") 'electric-case-trigger)
  )

(defconst electric-case-java-exeptions
  '("boolean" "char" "byte" "short" "int" "long" "float" "double" "void"
    "class" "public" "private" "protected" "static" "final" "abstract"
    "strictfp" "transient" "volatile" "new"))

(defun electric-case-java-init ()

  (electric-case-mode 1)

  (setq electric-case-criteria
        (lambda (b e)
          (when (not (member (buffer-substring b e) electric-case-java-exeptions))
            (let ((proper (text-properties-at b))
                  (key (key-description (this-single-command-keys))))
              (cond ((member 'font-lock-function-name-face proper) 'ucamel)
                    ((member 'font-lock-variable-name-face proper) 'camel)
                    ((member 'font-lock-type-face proper) 'ucamel)
                    (t nil))))))

  (define-key electric-case-mode-map (kbd "SPC") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd "(") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd ";") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd ",") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd "{") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd "=") 'electric-case-trigger)
  (define-key electric-case-mode-map (kbd ".") 'electric-case-trigger)
  )

;; * provide

(provide 'electric-case)

;;; electric-case.el ends here
