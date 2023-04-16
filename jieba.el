;;; jieba.el --- Emacs binding for jieba-rs -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Maintainer: Kisaragi Hiu
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.19.1"))
;; Homepage: https://github.com/kisaragi-hiu/emacs-jieba
;; Keywords: extensions

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

;; Emacs Lisp bindings for jieba-rs.

;;; Code:

(require 'dash)
(require 'ansi-color)
(require 'json)

(defgroup jieba nil
  "Emacs Lisp bindings for jieba-rs."
  :group 'extensions)

(defcustom jieba-dyn-dir (file-name-directory
                          (or (locate-library "jieba-dyn")
                              (locate-library "jieba")
                              load-file-name
                              buffer-file-name))
  "The directory where the `jieba-dyn' module is, or should be.

This needs to be set before `jieba' is loaded."
  :group 'jieba
  :type 'directory)

(defcustom jieba-dyn-get-method (list "todo" 'compile)
  "How to get the dynamic module for jieba, if necessary.

This needs to be set before `jieba' is loaded.

A string means to download it from this URL.

The symbol `compile' means to automatically compile it with a
local Rust toolchain."
  :group 'jieba
  :type '(set (choice
               (const :tag "Binary from GitHub" "todo")
               (string :tag "Another URL"))
              (const :tag "Local compilation" 'compile)))

(defmacro jieba--with-temp-dir (&rest body)
  "BODY with `tmp-dir' bound to a new temporary directory.
BODY needs to take care of deleting `tmp-dir' itself."
  (declare (indent 0))
  `(let ((tmp-dir (make-temp-file "emacs-jieba-build" t)))
     ,@body))

(defun jieba--dyn-get-ensure ()
  "Ensure `jieba-dyn' is available by downloading it or building it."
  (let ((load-path (cons jieba-dyn-dir load-path)))
    (unless (locate-library "jieba-dyn")
      (or (and (-some #'stringp jieba-dyn-get-method)
               (jieba--dyn-download
                (-first #'stringp jieba-dyn-get-method)))
          (and (memq 'compile jieba-dyn-get-method)
               (jieba--dyn-build)))
      ;; The library should be available now. If not, signal.
      (unless (locate-library "jieba-dyn")
        (error "Could not ensure `jieba-dyn' is available")))
    t))

(defun jieba--dyn-download--triple ()
  "Get the triple representing the current system."
  (with-temp-buffer
    (cond
     ((equal system-configuration "x86_64-unknown-linux-gnu")
      system-configuration)
     ((equal system-configuration "aarch64-unknown-linux-android")
      "aarch64-linux-android")
     ((eq system-type 'windows-nt)
      (when (stringp system-configuration)
        (and (string-match "x86_64" system-configuration)
             "x86_64-pc-windows-gnu")))
     ((eq system-type 'darwin)
      (when (stringp system-configuration)
        (or (and (string-match "x86_64" system-configuration)
                 "x86_64-apple-darwin")
            (and (string-match "aarch64" system-configuration)
                 "aarch64-apple-darwin"))))
     ((executable-find "rustc")
      (save-excursion
        (call-process "rustc" nil '(t nil) nil "-Vv"))
      (save-match-data
        (search-forward "host: " nil t))
      (buffer-substring-no-properties
       (point) (line-end-position))))))

system-configuration

system-type

(defun jieba--dyn-download--triple--uname--os (uname-output)
  "Return OS from UNAME-OUTPUT."
  (cl-block nil
    (pcase-dolist (`(,code . ,needles)
                   '(("windows")
                     ("android")
                     ("linux" "Linux")
                     ("darwin" "Darwin")
                     ("freebsd" "FreeBSD")
                     ("openbsd" "OpenBSD")
                     ("netbsd" "NetBSD")
                     ("dragonfly" "DragonFly")))
      (when (--any? (string-match it uname-output) needles)
        (cl-return code)))
    (warn "uname does not contain a known OS")
    (cl-return "unknown")))
(defun jieba--dyn-download--triple--uname--architecture (uname-output)
  "Return architecture from UNAME-OUTPUT."
  (cl-block nil
    (pcase-dolist (`(,code . ,needle)
                   '(("arm" "arm")
                     ("armv7" "armv7l")
                     ("armv8" "armv8l")
                     ("aarch64" "aarch64" "arm64")
                     ("i686" "i686")
                     ("x86_64" "x86_64")
                     ("mips" "mips")
                     ("mips64" "mips64")
                     ("powerpc" "powerpc")
                     ("powerpc64" "powerpc64")))
      (when (string-match needle uname-output)
        (cl-return code)))
    (warn "uname does not contain a known architecture")
    (cl-return "unknown")))

(jieba--dyn-download--triple)

(defun jieba--dyn-download (_url)
  "Download the built dynamic module from URL."
  nil)

(defun jieba--dyn-build (&optional target)
  "Build the dynamic library.
When TARGET is non-nil, try to cross-build for TARGET.
When the \"jieba_target\" environment variable is set, use that
as TARGET instead.

Note that currently TARGET needs to be the same operating system
as the host."
  (unless (executable-find "cargo")
    (error "Rust toolchain is not available. Make sure `cargo' is in your path"))
  (when (getenv "jieba_target")
    (setq target (getenv "jieba_target")))
  (jieba--with-temp-dir
    (copy-directory "." tmp-dir nil t t)
    (let ((default-directory tmp-dir)
          (buf (pop-to-buffer (get-buffer-create "*cargo build*")))
          pipe)
      (with-current-buffer buf
        (compilation-mode))
      (setq
       pipe
       (make-pipe-process
        :name "cargo output"
        :buffer nil
        :filter (lambda (_process output)
                  (with-current-buffer buf
                    (let ((inhibit-read-only t)
                          (output (ansi-color-apply (format "%s" output))))
                      (goto-char (point-max))
                      (if noninteractive
                          (princ output)
                        (insert output)))))
        :sentinel (lambda (process _change)
                    (when (eq 'closed (process-status process))
                      (kill-buffer buf)))))
      (let ((json-buf (generate-new-buffer " *json*"))
            (output-file (expand-file-name (concat "jieba-dyn" module-file-suffix)
                                           jieba-dyn-dir))
            process
            cargo-output-file)
        (setq process
              (make-process
               :name "cargo"
               :stderr pipe
               :command `("cargo" "build" "-r"
                          "--color" "always"
                          "--message-format" "json"
                          ,@(when target
                              `(("--target" ,target))))
               :sentinel (lambda (process _change)
                           (when (eq 'exit (process-status process))
                             (message "Compiled %s" output-file)
                             (kill-buffer json-buf)
                             (delete-directory tmp-dir t)))
               :filter
               (lambda (_process output)
                 (with-current-buffer json-buf
                   (insert (format "%s" output))
                   (goto-char (point-min))
                   (while (search-forward "\n" nil t)
                     (goto-char (point-min))
                     (let ((json (ignore-errors (json-read))))
                       (when json
                         (let-alist json
                           (when (and (equal .reason "compiler-artifact")
                                      (equal .target.name "emacs-jieba"))
                             (setq cargo-output-file (elt .filenames 0))))))
                     (delete-region (point-min) (point))))
                 (when cargo-output-file
                   (copy-file cargo-output-file
                              output-file
                              :ok)))))
        ;; Block until it's done.
        (while (accept-process-output process)
          (redisplay))))))

(jieba--dyn-get-ensure)

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
