;; writegood-mode.el --- Count number of words in a buffer
;;
;; Author: Benjamin Beckwith
;; Created: 2010-8-12
;; Version: 1.2
;; Last-Updated: 2010-8-13
;; URL: http://github.com/bnbeckwith/writegood-mode
;; Keywords: writing weasel-words grammar
;; Compatability:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This minor mode tries to find and highlight problems with your
;;  writing (in english).
;;
;;  Behavior inspired by the weaselwords scripts to aid in good
;;  writing.
;;  http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1.2 Fixed weasel-words regexp to have word boundaries
;; 1.1 Fixed regexps to be multiline.
;; 1.0 Initial version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Test Text:
;;
;; This mode will improve various aspects of your writing in many ways.
;; With this mode text within comments will be searched for the
;; the duplicate propblem.
;; The text is searched and aspects (even withing comments) are 
;; highlighted. 
;; Another benefit is the the finding of duplicates.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'regexp-opt)
(require 'faces)

(defgroup writegood nil
  "Minor mode for highlighting bad english writing."
  :prefix "writegood-"
  :group 'help
  :link '(url-link "http://github.com/bnbeckwith/writegood-mode"))

(defconst writegood-version "1.2"
  "WriteGood mode version")

;; Weaselwords
(defface writegood-weasels-face
  '((((class color) (background light))
     (:inherit font-lock-warning-face :background "moccasin"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DarkOrange")))
  "Writegood face for weasel words"
  :group 'writegood)

(defcustom writegood-weasel-words
  '("many" "various" "very" "fairly" "several" "extremely" 
    "exceedingly" "quite" "remarkably" "few" "surprisingly" 
    "mostly" "largely" "huge" "tiny" "are a number" "is a number" 
    "excellent" "interestingly" "significantly" "substantially" 
    "clearly" "vast" "relatively" "completely")
  "The weasel words to use"
  :group 'writegood
  :type 'list)
  
(defvar writegood-weasels-font-lock-keywords-regexp
  (concat "\\b" (regexp-opt writegood-weasel-words) "\\b")
  "Matches weasel-words")

(defvar writegood-weasels-font-lock-keywords
  (list (list writegood-weasels-font-lock-keywords-regexp
	      0 (quote 'writegood-weasels-face) 'prepend)))

;; Passive Voice
(defface writegood-passive-voice-face
  '((((class color))
     (:inherit font-lock-warning-face :background "LemonChiffon")))
  "Writegood face for passive-voice"
  :group 'writegood)

(defcustom writegood-passive-voice-irregulars
  '("awoken" "been" "born" "beat" "become" "begun" "bent" "beset" 
    "bet" "bid" "bidden" "bound" "bitten" "bled" "blown" "broken" 
    "bred" "brought" "broadcast" "built" "burnt" "burst" "bought" 
    "cast" "caught" "chosen" "clung" "come" "cost" "crept" "cut" 
    "dealt" "dug" "dived" "done" "drawn" "dreamt" "driven" "drunk" 
    "eaten" "fallen" "fed" "felt" "fought" "found" "fit" "fled" 
    "flung" "flown" "forbidden" "forgotten" "foregone" "forgiven" 
    "forsaken" "frozen" "gotten" "given" "gone" "ground" "grown" 
    "hung" "heard" "hidden" "hit" "held" "hurt" "kept" "knelt" "knit" 
    "known" "laid" "led" "leapt" "learnt" "left" "lent" "let" "lain" 
    "lighted" "lost" "made" "meant" "met" "misspelt" "mistaken" "mown" 
    "overcome" "overdone" "overtaken" "overthrown" "paid" "pled" "proven" 
    "put" "quit" "read" "rid" "ridden" "rung" "risen" "run" "sawn" 
    "said" "seen" "sought" "sold" "sent" "set" "sewn" "shaken" "shaven" 
    "shorn" "shed" "shone" "shod" "shot" "shown" "shrunk" "shut" 
    "sung" "sunk" "sat" "slept" "slain" "slid" "slung" "slit" 
    "smitten" "sown" "spoken" "sped" "spent" "spilt" "spun" "spit" 
    "split" "spread" "sprung" "stood" "stolen" "stuck" "stung" 
    "stunk" "stridden" "struck" "strung" "striven" "sworn" "swept" 
    "swollen" "swum" "swung" "taken" "taught" "torn" "told" "thought" 
    "thrived" "thrown" "thrust" "trodden" "understood" "upheld" "upset" 
    "woken" "worn" "woven" "wed" "wept" "wound" "won" "withheld" 
    "withstood" "wrung" "written")
  "List of passive voice irregular verbs"
  :group 'writegood
  :type 'list)

(defvar writegood-passive-voice-font-lock-keywords-regexp
  (concat "\\b\\(am\\|are\\|were\\|being\\|is\\|been\\|was\\|be\\)\\b\\([[:space:]]\\|\\s<\\|\\s>\\)+\\([[:word:]]+ed\\|"
	  (regexp-opt writegood-passive-voice-irregulars)
	  "\\)")
  "Font-lock keywords regexp for passive-voice")

(defvar writegood-passive-voice-font-lock-keywords
  (list (list writegood-passive-voice-font-lock-keywords-regexp
	      0 (quote 'writegood-passive-voice-face) 'prepend)))

;; Duplicates
(defface writegood-duplicates-face
  '((((class color) (background light))
     (:inherit font-lock-warning-face :background "MistyRose"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DeepPink")))
  "Writegood face for duplicate words"
  :group 'writegood)

(defvar writegood-duplicates-font-lock-keywords-regexp
  "\\b\\([[:word:]]+\\)\\([[:space:]]\\|\\s<\\|\\s>\\)+\\1\\b"
  "Font-lock keywords for duplicates")

(defvar writegood-duplicates-font-lock-keywords
  (list (list writegood-duplicates-font-lock-keywords-regexp
	      0 (quote 'writegood-duplicates-face) 'prepend)))

;;;;;;;;;;;;;;;;;;;; Functions:

(defun writegood-version ()
  "Tell the version you are using"
  (interactive)
  (message writegood-version))

(defun writegood-weasels-turn-on ()
  "Turn on syntax highlighting for weasels"
  (font-lock-add-keywords nil writegood-weasels-font-lock-keywords))

(defun writegood-passive-voice-turn-on ()
  "Turn on warnings for passive voice"
  (font-lock-add-keywords nil writegood-passive-voice-font-lock-keywords))

(defun writegood-duplicates-turn-on ()
  "Turn on warnings for duplicate words"
  (font-lock-add-keywords nil writegood-duplicates-font-lock-keywords))

(defun writegood-weasels-turn-off ()
  "Turn on syntax highlighting for weasels"
  (font-lock-remove-keywords nil writegood-weasels-font-lock-keywords))

(defun writegood-passive-voice-turn-off ()
  "Turn on warnings for passive voice"
  (font-lock-remove-keywords nil writegood-passive-voice-font-lock-keywords))

(defun writegood-duplicates-turn-off ()
  "Turn on warnings for duplicate words"
  (font-lock-remove-keywords nil writegood-duplicates-font-lock-keywords))

(defun writegood-turn-on ()
  "Turn on writegood-mode."
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (writegood-weasels-turn-on)
  (writegood-passive-voice-turn-on)
  (writegood-duplicates-turn-on))

(defun writegood-turn-off ()
  "Turn off writegood-mode."
  (writegood-weasels-turn-off)
  (writegood-passive-voice-turn-off)
  (writegood-duplicates-turn-off))

(define-minor-mode writegood-mode
  "Colorize issues with the writing in the buffer."
  :lighter " Wg"
  (progn
    (if writegood-mode
	(writegood-turn-on)
      (writegood-turn-off))
    (font-lock-mode 1)))

(provide 'writegood-mode)