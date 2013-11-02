;;; color-theme-convert.el --- convert color-theme to deftheme

;; Copyright (C) 2013  George Kettleborough

;; Author: George Kettleborough <g.kettleborough@member.fsf.org>
;; Created: 20131026
;; Version: 0.1.0
;; Status: experimental
;; Package-Requires: ((cl-lib "0.2"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Converts color-theme themes to the current deftheme type.

;;; Code:

(require 'cl-lib)

(defvar color-theme-convert-obarray)

(defmacro intern-obs (name)
  `(intern ,name color-theme-convert-obarray))

(defun color-theme-convert (filein)
  (interactive
   (list (read-file-name "File in: ")))
  (with-temp-buffer
    (insert-file-contents filein)
    (color-theme-convert-buffer (current-buffer)
                                (get-buffer-create "*test*"))))

;;; note we set up a new obarray to avoid polluting the current obarray with
;;; stuff from `read'
(defun color-theme-convert-buffer (buffer)
  (interactive
   (list (current-buffer)))
  (let ((color-theme-convert-obarray (make-vector 1511 0))
        (forms-left t)
        (form nil)
        (converted-forms (list)))
    (with-current-buffer buffer
      (beginning-of-buffer)
      (while forms-left
        (condition-case nil
            (let ((obarray color-theme-convert-obarray))
             (setq form (read buffer)))
          (error (setq forms-left nil)))
        (when (color-theme-convert-defun-p form)
          (push (color-theme-convert-form form) converted-forms))
        (setq form nil))
      ;; print converted forms
      (erase-buffer)
      (cl-dolist (converted-form converted-forms)
        (insert (format ";;; %s theme, converted automatically using \
color-theme-convert\n"
                        (cl-cadar converted-form)))
        (cl-dolist (form converted-form)
          (pp form buffer))))))

(defun color-theme-convert-defun-p (form)
  (and (eq (intern-obs "defun" ) (car form))
       (symbolp (cadr form))
       (string= (substring (symbol-name (cadr form)) 0 11) "color-theme")))

(defun color-theme-convert-name (name)
  (let ((name-split (split-string name "-")))
    (mapconcat #'identity
               (if (and (string= (car name-split) "color")
                        (string= (cadr name-split) "theme"))
                   (cddr name-split)
                 name-split)
               "-")))

(defun color-theme-convert-quoted (form)
  (list 'quote form))

(defun color-theme-convert-default-face (default-face frame-params)
  (let ((face-plist (cadr (assq (intern-obs "t") default-face)))
        (foreground (cdr (assq (intern-obs "foreground-color") frame-params)))
        (background (cdr (assq (intern-obs "background-color") frame-params))))
    (setq face-plist (remq (intern-obs "nil") face-plist))
    (when foreground
      (setq face-plist
            (plist-put face-plist (intern-obs ":foreground") foreground)))
    (when background
      (setq face-plist
            (plist-put face-plist (intern-obs ":background") background)))
    (setcar (cdr (assq (intern-obs "t") default-face)) face-plist)
    default-face))

(defun color-theme-convert-form (form)
  (let* ((theme-form (cl-cadadr
                      (cl-find-if
                       (lambda (f) (and (consp f)
                                        (eq (car f)
                                            (intern-obs
                                             "color-theme-install"))))
                       form)))
         name frame-params variables faces)
    ;; get pieces (based on `color-theme-canonic')
    (setq name (intern-obs (color-theme-convert-name
                            (symbol-name (pop theme-form)))))
    (setq frame-params (pop theme-form))
    (when (listp (caar theme-form))
      (setq variables (pop theme-form)))
    (setq faces theme-form)
    ;; put frame-params in default face
    (setcar (cdr (assq (intern-obs "default") faces))
            (color-theme-convert-default-face
             (cadr (assq (intern-obs "default") faces))
             frame-params))
    ;; set up deftheme form
    (list `(deftheme ,name)
          `(custom-theme-set-faces
            ',name
            ,@(mapcar #'color-theme-convert-quoted faces))
          `(custom-theme-set-variables
            ',name
            ,@(mapcar #'color-theme-convert-quoted variables))
          `(provide-theme ',name))))

(defmacro ctc-doplist (spec &rest body)
  "Loop over a plist.
Evaluate BODY with KEY and VAL bound to each pair from PLIST, in
turn.

\(fn (KEY VAL PLIST) BODY...)"
  (declare (indent 1))
  (let ((temp '--ctc-doplist-tail--)
        (key (nth 0 spec))
        (val (nth 1 spec))
        (plist (nth 2 spec)))
    `(let ((,temp ,plist)
           ,key
           ,val)
       (while ,temp
         (setq ,key (car ,temp))
         (if (cdr ,temp)
             (setq ,temp (cdr ,temp) ,val (car ,temp))
           (setq ,val nil))
         ,@body
         (setq ,temp (cdr ,temp))))))


(provide 'color-theme-convert)
;;; color-theme-convert.el ends here
