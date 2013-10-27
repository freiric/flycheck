;;; flycheck-steps.el --- Flycheck: Step definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Step definitions

;;; Code:

(require 'ecukes)
(require 'espuds)

(require 'dash)
(require 'f)

(require 'cl-lib)

(require 'flycheck)

(defun flycheck-steps-format-error (err)
  (let ((filename (flycheck-error-filename err))
        (checker (flycheck-error-checker err))
        (level (flycheck-error-level err))
        (line (flycheck-error-line err))
        (column (flycheck-error-column err))
        (message (flycheck-error-message err)))
    (when filename
      (setq filename (f-relative filename flycheck-env-current-temp-dir)))
    (s-lex-format "${filename}:${checker}:${level}:${line}:${column}:${message}")))

(defun flycheck-steps-format-errors (errors)
  (s-join "\n" (-map #'flycheck-steps-format-error errors)))

(defun flycheck-steps-error-from-row (header row)
  "Create an error from HEADER and ROW."
  (let* ((keywords (--map (intern (concat ":" it)) header))
         (args (-flatten (-zip-with #'list keywords row)))
         (error (apply #'flycheck-error-new args)))
    (-when-let (message (flycheck-error-message error))
      (setf (flycheck-error-message error)
            (s-replace "\\n" "\n" message)))
    (-when-let (line (flycheck-error-line error))
      (setf (flycheck-error-line error) (string-to-number line)))
    (-when-let (column (flycheck-error-column error))
      (setf (flycheck-error-column error) (string-to-number column)))
    (-when-let (level (flycheck-error-level error))
      (setf (flycheck-error-level error) (intern level)))
    (-when-let (checker (flycheck-error-checker error))
      (setf (flycheck-error-checker error) (intern checker)))
    (-when-let (filename (flycheck-error-filename error))
      (setf (flycheck-error-filename error)
            (f-join flycheck-env-current-temp-dir filename)))
    (setf (flycheck-error-buffer error) (current-buffer))
    error))

(Given "^a buffer \"\\(.+\\)\" in \\(.+\\):$"
  (lambda (filename mode text)
    (find-file (flycheck-env-create-file filename text))
    (When "I turn on %s" mode)))

(Given "^a file \"\\(.+\\)\":"
  (lambda (filename text)
    (flycheck-env-create-file filename text)))

(When "^I disable checker \\(.+\\)$"
  (lambda (checker)
    (unless (local-variable-p 'flycheck-checkers)
      (setq-local flycheck-checkers (copy-seq flycheck-checkers)))
    (setq flycheck-checkers (delq (intern checker) flycheck-checkers))))

(When "^I check syntax$"
  (lambda (callback)
    (flycheck-mode 1)
    (add-hook 'flycheck-after-syntax-check-hook callback nil 'local)
    (cl-assert (flycheck-get-checker-for-buffer))
    (flycheck-buffer)))

(Then "^I should see Flycheck errors:$"
  (lambda (errors)
    (let* ((header (car errors))
           (errors (--map (flycheck-steps-error-from-row header it) (cdr errors))))
      (cl-assert (equal errors flycheck-current-errors) nil
                 "Unexpected errors:\n%s" (flycheck-steps-format-errors flycheck-current-errors)))))

(Then "^I should see no Flycheck errors$"
  (lambda ()
    (cl-assert (equal nil flycheck-current-errors) nil
               "Unexpected errors:\n%s" (flycheck-steps-format-errors flycheck-current-errors))))

(provide 'flycheck-steps)

;;; flycheck-steps.el ends here
