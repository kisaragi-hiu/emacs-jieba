;;; jieba.el --- Emacs binding for jieba-rs -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Maintainer: Kisaragi Hiu
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'jieba-dyn)

;; jieba-load

(defun jieba-add-word (word &optional pos frequency)
  "Add WORD to the current Jieba instance's dictionary.
POS is the part of speech (\"tag\") of WORD.
FREQUENCY, if non-nil, gives Jieba more information to work with."
  (jieba--add-word word (or pos "n") frequency))

(defun jieba-cut (sentence &optional hmm)
  "Cut SENTENCE into a vector of words.
HMM: enable word discovery."
  (jieba--cut sentence hmm))

;; jieba-cut-all

(defun jieba-cut-for-search (sentence &optional hmm)
  "Cut SENTENCE into a vector of words for search purposes.
Overlapping substrings are also returned to aid search engines.
HMM: enable word discovery."
  (jieba--cut sentence hmm))

(defun jieba-tokenize (sentence &optional hmm mode)
  "Tokenize SENTENCE.
HMM: enable word discovery.
MODE: `search' means to use search mode, otherwise use default mode"
  (jieba--tokenize sentence
                   (pcase mode
                     ('search "search")
                     (_ "default"))
                   hmm))

(defun jieba-tag (sentence &optional hmm)
  "Cut SENTENCE into tokens along with parts of speech information.

HMM: enable word discovery.

Return results in the format [(WORD . TAG) ...]."
  (jieba--tag sentence hmm))

(defun jieba-extract (sentence n &optional allowed-pos)
  "Extract the top N keywords from SENTENCE.
ALLOWED-POS is a comma-separated list of POS specifiers. If nil,
Jieba's default is used."
  (jieba--extract sentence n allowed-pos))

(defun jieba-extract-keywords (sentence n &optional allowed-pos)
  "Extract the top N keywords from SENTENCE.
Like `jieba-extract', but weights are discarded.
ALLOWED-POS is a comma-separated list of POS specifiers. If nil,
Jieba's default is used."
  (jieba--extract-keywords sentence n allowed-pos))

(provide 'jieba)

;;; jieba.el ends here
