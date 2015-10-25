;;; gypsum-mode.el --- Syntax highlighting for Gypsum

;; Copyright 2014-2015, Jay Conrod, all rights reserved.

;; Author: Jay Conrod <jayconrod@gmail.com>
;; Description: Syntax highlighting for Gypsum
;; Keywords: gypsum mode
;; Version: 0.1

;; This file is NOT part of GNU Emacs

;; License
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:
;;
;; This file provides very basic syntax highlighting for the Gypsum
;; programming language. It is very much a work in progress.

;;; Installation:
;; Once this package is installed, add this to your .emacs:
;;
;;   (require 'gypsum-mode)
;;   (add-to-list 'auto-mode-alist '("\\.gy$" . gypsum-mode))

;;; Code:

(setq gypsum-keywords-regexp (regexp-opt
  '("abstract"
    "arrayelements"
    "as"
    "break"
    "case"
    "catch"
    "class"
    "continue"
    "def"
    "else"
    "final"
    "finally"
    "if"
    "import"
    "lambda"
    "let"
    "match"
    "new"
    "private"
    "protected"
    "public"
    "return"
    "static"
    "super"
    "this"
    "throw"
    "trait"
    "try"
    "var"
    "while")))

(setq gypsum-type-keywords-regexp (regexp-opt
  '("boolean"
    "f32"
    "f64"
    "i8"
    "i16"
    "i32"
    "i64"
    "unit")))

(setq gypsum-constant-keywords-regexp (regexp-opt
  '("false"
    "null"
    "true"
    "{}")))

(setq gypsum-patterns
  `(("//.*$" . font-lock-comment-face)
    (,gypsum-constant-keywords-regexp . font-lock-constant-face)
    (,gypsum-type-keywords-regexp . font-lock-type-face)
    ("\\<[+-]?[0-9]+\\.[0-9]*\\(?:[eE][+-]?[0-9]+\\)?\\>" . font-lock-constant-face)
    ("\\<[+-]?\\.[0-9]+\\(?:[eE][+-]?[0-9]+\\)?\\>" . font-lock-constant-face)
    ("\\<[+-]?[0-9]+\\(?:[eE][+-]?[0-9]+\\)?\\>" . font-lock-constant-face)
    ("\"[^\"]*?\"" . font-lock-string-face)
    ("\\(?:def\\|let\\|var\\|class\\) *\\([A-Za-z0-9_-]+\\)" 1 font-lock-variable-name-face)
    (,gypsum-keywords-regexp . font-lock-keyword-face)))


(define-derived-mode gypsum-mode fundamental-mode
  (setq font-lock-defaults '(gypsum-patterns))
  (setq mode-name "Gypsum"))


(add-to-list 'auto-mode-alist '("\\.gy$" . gypsum-mode))


(provide 'gypsum-mode)

;;; gypsum-mode.el ends here
