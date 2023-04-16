;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(defvar jieba-dyn-get-method)
(setq jieba-dyn-get-method nil)
;; Ignore ensure failure
(ignore-errors
  (require 'jieba nil t))

(message "system configuration: %S" system-configuration)
(message "computed triple: %S" (jieba--dyn-download--triple))
