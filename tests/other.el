;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(defvar jieba-dyn-get-method)
(setq jieba-dyn-get-method nil)
(require 'jieba)

(message "system configuration: %S" system-configuration)
(message "computed triple: %S" (jieba--dyn-download--triple))
