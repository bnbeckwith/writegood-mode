;;; writegood-mode.el --- Polish up poor writing on the fly
;;
;; Author: Benjamin Beckwith
;; Created: 2010-8-12
;; Version: 2.0
;; Last-Updated: 2015-03-25
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
;; 2.0.4 Remove cl dependency
;; 2.0.3 Add in a small decription of the Flesch-Kincaid score
;; 2.0.2 Fix Formatting in Org-mode files, make faces underline
;; 2.0.1 Make user additions to word lists dynamic
;; 2.0.0 Flesch-Kincaid scoring added to functionality
;; 1.3.0 Several pull requests added, comments checked, passive voice regexp fixed
;; 1.2.0 Fixed weasel-words regexp to have word boundaries
;; 1.1.0 Fixed regexps to be multiline.
;; 1.0.0 Initial version
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
;; With this mode, text within comments will be searched for the
;; the duplicate problem.
;; The text is searched and aspects (even within comments) are
;; highlighted.
;; Another benefit is the the finding of duplicates.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'regexp-opt)
(require 'faces)

(defgroup writegood nil
  "Minor mode for highlighting bad english writing."
  :prefix "writegood-"
  :group 'help
  :link '(url-link "http://github.com/bnbeckwith/writegood-mode"))

(defconst writegood-version "2.0"
  "WriteGood mode version")

;; Weaselwords
(defface writegood-weasels-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (((class color) (background light))
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
    "clearly" "vast" "relatively" "completely" "literally"
    "not rocket science" "outside the box")
  "The weasel words to use"
  :group 'writegood
  :type '(repeat string))

(defun writegood-weasels-font-lock-keywords-regexp ()
  "Generate regex that matches weasel-words"
  (concat "\\b" (regexp-opt writegood-weasel-words) "\\b"))

(defun writegood-weasels-font-lock-keywords ()
  (list (list (writegood-weasels-font-lock-keywords-regexp)
        0 (quote 'writegood-weasels-face) 'prepend)))

;; Passive Voice
(defface writegood-passive-voice-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "cyan"))
    (((class color))
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
  :type '(repeat string))

(defcustom writegood-sentence-punctuation
  '(?. ?? ?!)
  "List of punctuation denoting sentence end"
  :group 'writegood
  :type '(repeat character))

(defun writegood-passive-voice-font-lock-keywords-regexp ()
  "Generate font-lock keywords regexp for passive-voice"
  (concat "\\b\\(am\\|are\\|were\\|being\\|is\\|been\\|was\\|be\\)\\b\\([[:space:]]\\|\\s<\\|\\s>\\)+\\([[:word:]]+ed\\|"
    (regexp-opt writegood-passive-voice-irregulars)
    "\\)\\b"))

(defun writegood-passive-voice-font-lock-keywords ()
  (list (list (writegood-passive-voice-font-lock-keywords-regexp)
        0 (quote 'writegood-passive-voice-face) 'prepend)))

;; Duplicates
(defface writegood-duplicates-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DeepPink"))
    (((class color) (background light))
        (:inherit font-lock-warning-face :background "MistyRose"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DeepPink")))
  "Writegood face for duplicate words"
  :group 'writegood)

(defvar writegood-duplicates-font-lock-keywords-regexp
  "\\b\\([[:word:]]+\\)\\([[:space:]]\\|\\s<\\|\\s>\\)+\\1\\b"
  "Font-lock keywords for duplicates")

(defun writegood-duplicates-font-lock-keywords ()
  (list (list writegood-duplicates-font-lock-keywords-regexp
        0 (quote 'writegood-duplicates-face) 'prepend)))

;;;;;;;;;;;;;;;;;;;; Functions:

(defun writegood-version ()
  "Tell the version you are using"
  (interactive)
  (message writegood-version))

(defun writegood-weasels-turn-on ()
  "Turn on syntax highlighting for weasels"
  (font-lock-add-keywords nil (writegood-weasels-font-lock-keywords) t))

(defun writegood-passive-voice-turn-on ()
  "Turn on warnings for passive voice"
  (font-lock-add-keywords nil (writegood-passive-voice-font-lock-keywords) t))

(defun writegood-duplicates-turn-on ()
  "Turn on warnings for duplicate words"
  (font-lock-add-keywords nil (writegood-duplicates-font-lock-keywords) t))

(defun writegood-weasels-turn-off ()
  "Turn on syntax highlighting for weasels"
  (font-lock-remove-keywords nil (writegood-weasels-font-lock-keywords)))

(defun writegood-passive-voice-turn-off ()
  "Turn on warnings for passive voice"
  (font-lock-remove-keywords nil (writegood-passive-voice-font-lock-keywords)))

(defun writegood-duplicates-turn-off ()
  "Turn on warnings for duplicate words"
  (font-lock-remove-keywords nil (writegood-duplicates-font-lock-keywords)))

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

(defun writegood-count-words (rstart rend)
  "Count the words specified by the region bounded by RSTART and REND."
  (if (boundp 'count-words)
      (count-words rstart rend)
    (how-many "[[:word:]]+" rstart rend)))

(defun writegood-count-sentences (rstart rend)
  "Count the sentences specified by the region bounded by RSTART and REND."
  (how-many (regexp-opt-charset writegood-sentence-punctuation) rstart rend))

(defun writegood-count-syllables (rstart rend)
  "Count the (approximate) number of syllables in the region bounded by RSTART and REND.

   Consecutive vowels count as one syllable. The endings -es -ed
   and -e are not counted as syllables.
  "
  (- (how-many "[aeiouy]+" rstart rend)
     (how-many "\\(es\\|ed\\|e\\)\\b" rstart rend)))

(defun writegood-fk-parameters (&optional rstart rend)
  "Flesch-Kincaid reading parameters"
  (let* ((start (cond (rstart rstart)
                      ((and transient-mark-mode mark-active) (region-beginning))
                      ('t (point-min))))
         (end   (cond (rend rend)
                      ((and transient-mark-mode mark-active) (region-end))
                      ('t (point-max))))
         (words     (float (writegood-count-words start end)))
         (syllables (float (writegood-count-syllables start end)))
         (sentences (float (writegood-count-sentences start end))))
    (list sentences words syllables)))

(defun writegood-reading-ease-score->comment (score)
  "Rough interpreation of the Flesch-Kincaid Reading ease SCORE.

From Wikipedia URL `https://en.wikipedia.org/wiki/Flesch–Kincaid_readability_tests'."
   (cond
    ((< score 0) "Ouch! (Proust literature)")
    ((and (<= 0 score) (< score 30.0)) "Very difficult (college graduate)")
    ((and (<= 30.0 score) (< score 50.0)) "Difficult (almost college)")
    ((and (<= 50.0 score) (< score 60.0)) "Fairly difficult (10-12th grade)")
    ((and (<= 60.0 score) (< score 70.0)) "Plain English (8-9th grade)")
    ((and (<= 70.0 score) (< score 80.0)) "Fairly easy (7th grade)")
    ((and (<= 80.0 score) (< score 90.0)) "Easy (6th grade)")
    ((<= 90.0 score) "Very easy (5th grade)")))

;;;###autoload
(defun writegood-reading-ease (&optional start end)
  "Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100."
   (interactive)
   (let* ((params (writegood-fk-parameters start end))
          (sentences (nth 0 params))
          (words     (nth 1 params))
          (syllables (nth 2 params))
          (score  (- 206.835 (* 1.015 (/ words sentences)) (* 84.6 (/ syllables words)))))
     (message "Flesch-Kincaid reading ease score: %.2f %s" score
            (writegood-reading-ease-score->comment score))))

;;;###autoload
(defun writegood-grade-level (&optional start end)
  "Flesch-Kincaid grade level test. Converts reading ease score to a grade level (Score ~ years of school needed to read passage)."
   (interactive)
   (let* ((params (writegood-fk-parameters start end))
          (sentences (nth 0 params))
          (words     (nth 1 params))
          (syllables (nth 2 params))
          (score     (+ (* 0.39 (/ words sentences)) (* 11.8 (/ syllables words)) -15.59)))
     (message "Flesch-Kincaid grade level score: %.2f" score)))

;;;###autoload
(define-minor-mode writegood-mode
  "Colorize issues with the writing in the buffer."
  :lighter " Wg"
  (progn
    (if writegood-mode
  (writegood-turn-on)
      (writegood-turn-off))
    (font-lock-mode 1)))

(provide 'writegood-mode)

;;; writegood-mode.el ends here
