;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'buttercup)
(require 'jieba)

(defvar test:sample-1
  "軌道逐漸往下，進到地下的區間。還沒有電車通過。稍微再往前會有一個站，我們朝著那裡輕聲前進。")

(describe "Jieba"
  (it "cuts with word discovery"
    (expect
     (jieba-cut test:sample-1 t)
     :to-equal
     ["軌道" "逐漸" "往" "下" "，" "進到" "地下" "的" "區間" "。" "還沒有" "電車" "通過" "。" "稍微" "再" "往前" "會有" "一個" "站" "，" "我們" "朝著" "那裡" "輕聲" "前進" "。"]))
  (it "cuts"
    (expect
     (jieba-cut test:sample-1)
     :to-equal
     ["軌" "道" "逐" "漸" "往" "下" "，" "進" "到" "地下" "的" "區" "間" "。" "還" "沒" "有" "電" "車" "通" "過" "。" "稍微" "再" "往前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "輕" "聲" "前" "進" "。"]))
  (it "adds words"
    (jieba-add-word "軌道")
    (jieba-add-word "電車")
    (expect
     (jieba-cut test:sample-1)
     :to-equal
     ["軌道" "逐" "漸" "往" "下" "，" "進" "到" "地下" "的" "區" "間" "。" "還" "沒" "有" "電車" "通" "過" "。" "稍微" "再" "往前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "輕" "聲" "前" "進" "。"])))

(buttercup-run)
