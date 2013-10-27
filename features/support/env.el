;;; env.el --- Flycheck: Test environment            -*- lexical-binding: t; -*-

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

;; Test environment

;;; Code:

(require 'f)

(require 'cl-lib)

;; Load all modes used by our tests
(--each '(sh-script
          cc-mode
          coffee-mode
          css-mode
          d-mode
          elixir-mode
          erlang
          elixir-mode
          go-mode
          haml-mode
          haskell-mode
          web-mode
          js2-mode
          js3-mode
          less-css-mode
          lua-mode
          cperl-mode
          php-mode
          puppet-mode
          rust-mode
          sass-mode
          scala-mode2
          scss-mode
          slim-mode
          yaml-mode)
  (require it))

(eval-and-compile
  (defconst flycheck-source-dir (f-parent (f-parent (f-parent (f-this-file))))
    "Source directory to load Flycheck from."))

(require 'flycheck (f-join flycheck-source-dir "flycheck.elc"))

(let ((source (symbol-file 'flycheck-mode 'defun)))
  (when noninteractive
    (cl-assert (f-same? source (f-join flycheck-source-dir "flycheck.elc"))
               nil "ERROR: Flycheck not loaded from the byte compiled source, but from %s! \
Run make compile" source)))

(defvar flycheck-env-current-temp-dir nil
  "A directory for temporary files for the current test.")

(defun flycheck-env-create-file (filename contents)
  (let ((path (f-join flycheck-env-current-temp-dir filename)))
    (make-directory (f-parent path) 'parents)
    (with-temp-file path
      (insert contents))
    path))

(Before
 (setq flycheck-env-current-temp-dir (make-temp-file "flycheck-test" 'dir)))

(After
 (f-delete flycheck-env-current-temp-dir 'force)
 (setq flycheck-env-current-temp-dir nil)
 (kill-buffer))

;;; env.el ends here
