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

(defun color-theme-convert (filename)
  (interactive
   (list (read-file-name "File: ")))
  (with-temp-buffer
    (insert-file-contents filename)
    (color-theme-convert-buffer (current-buffer))))

;;; note we set up a new obarray to avoid polluting the current obarray with
;;; stuff from `read'
(defun color-theme-convert-buffer (buffer)
  (let ((obarray (make-vector 1511 0))
        (forms-left t)
        (form nil)
        (converted-forms (list)))
    (while forms-left
      (condition-case nil
          (setq form (read buffer))
        (error (setq forms-left nil)))
      (when (color-theme-convert-defun-p form)
        (push (color-theme-convert-form form) converted-forms))
      (setq form nil))
    converted-forms))

(defun color-theme-convert-defun-p (form)
  (and (eq (intern "defun") (car form))
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

(defun quoted (form)
  (list 'quote form))

(defun color-theme-convert-default-face (default-face frame-params)
  (let ((face-plist (cadr (assq (intern "t") default-face)))
        (foreground (cdr (assq (intern "foreground-color") frame-params)))
        (background (cdr (assq (intern "background-color") frame-params))))
    (setq face-plist (remq (intern "nil") face-plist))
    (when foreground
      (setq face-plist
            (plist-put face-plist (intern ":foreground") foreground)))
    (when background
      (setq face-plist
            (plist-put face-plist (intern ":background") background)))
    (setcar (cdr (assq (intern "t") default-face)) face-plist)
    default-face))

(defun color-theme-convert-form (form)
  (let* ((theme-form (cadadr
                      (cl-find-if
                       (lambda (f) (and (consp f)
                                        (eq (car f)
                                            (intern "color-theme-install"))))
                       form)))
         name frame-params variables faces)
    ;; get pieces (based on `color-theme-canonic')
    (setq name (intern (color-theme-convert-name
                        (symbol-name (pop theme-form)))))
    (setq frame-params (pop theme-form))
    (when (listp (caar theme-form))
      (setq variables (pop theme-form)))
    (setq faces theme-form)
    ;; put frame-params in default face
    (setcar (cdr (assq (intern "default") faces))
            (color-theme-convert-default-face
             (cadr (assq (intern "default") faces))
             frame-params))
    ;; set up deftheme form
    (list `(deftheme ,name)
          `(custom-theme-set-faces
            ',name
            ,@(mapcar #'quoted faces))
          `(custom-theme-set-variables
            ',name
            ,@(mapcar #'quoted variables))
          `(provide-theme ',name))))

(provide 'color-theme-convert)
;;; color-theme-convert.el ends here
