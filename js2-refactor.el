;; js2-refactor.el --- The beginnings of a JavaScript refactoring library in emacs.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

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

;; The beginnings of a JavaScript refactoring library in emacs.
;;
;; This is a collection of small refactoring functions to further the idea of a
;; JavaScript IDE in Emacs that started with js2-mode.
;;
;; ## Installation
;;
;; Start by installing the dependencies:
;;
;;  * js2-mode https://github.com/mooz/js2-mode/
;;  * mark-multiple https://github.com/magnars/mark-multiple.el
;;
;; It is also recommended to get
;; [expand-region](https://github.com/magnars/expand-region.el) to more easily mark
;; vars, method calls and functions for refactorings.
;;
;; Then add this to your emacs settings:
;;
;;     (require 'js2-refactor)
;;
;; ## Usage
;;
;; All refactorings start with `C-c C-m` and then a two-letter mnemonic shortcut.
;;
;;  * `eo` is `expand-object`: Converts a one line object literal to multiline.
;;  * `co` is `contract-object`: Converts a multiline object literal to one line.
;;  * `wi` is `wrap-buffer-in-iife`: Wraps the entire buffer in an immediately invoked function expression
;;  * `ig` is `inject-global-in-iife`: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;;  * `ev` is `extract-variable`: Takes a marked expression and replaces it with a var.
;;  * `rv` is `rename-var`: Renames the variable on point and all occurrences in its lexical scope.
;;
;; There are also some minor conveniences bundled:
;;
;;  * `C-S-down` and `C-S-up` moves the current line up or down. If the line is an
;;    element in an object or array literal, it makes sure that the commas are
;;    still correctly placed.
;;
;; ## Contribute
;;
;; This project is still in its infancy, and everything isn't quite sorted out
;; yet. If you're eager to contribute, please add an issue here on github and we
;; can discuss your changes a little before diving into the elisp. :-)

;;; Code:

(require 'js2-mode)

;;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar js2r-use-strict nil
  "When non-nil, js2r inserts strict declarations in IIFEs.")

;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-formatting)

;; Expand and contract object
(define-key js2-mode-map (kbd "C-c RET eo") 'js-expand-object)
(define-key js2-mode-map (kbd "C-c RET co") 'js-contract-object)



;;; Immediately invoked function expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-iife)

;; Wrap the entire buffer in an immediately invoked function expression
(define-key js2-mode-map (kbd "C-c RET wi") 'js-wrap-buffer-in-iife)

;; Create shortcut for marked global by injecting it in the wrapping IIFE
(define-key js2-mode-map (kbd "C-c RET ig") 'js-inject-global-in-iife)



;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-vars)

;; Extract
(define-key js2-mode-map (kbd "C-c RET ev") 'js2-extract-variable)

;; Rename
(define-key js2-mode-map (kbd "C-c RET rv") 'js2-rename-var)



;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-functions)

;; Toggle function declaration/expression
(define-key js2-mode-map (kbd "C-c RET tf") 'js2r-toggle-function-expression-and-declaration)



;;; Conveniences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-conveniences)

;; Make sure commas are placed correctly when moving a line in a literal
(define-key js2-mode-map (kbd "<C-S-down>") 'js-move-line-down)
(define-key js2-mode-map (kbd "<C-S-up>") 'js-move-line-up)



(provide 'js2-refactor)
