;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'buttercup)
(defvar jieba-dyn-get-method)
(setq jieba-dyn-get-method '(compile))
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
     ["軌道" "逐" "漸" "往" "下" "，" "進" "到" "地下" "的" "區" "間" "。" "還" "沒" "有" "電車" "通" "過" "。" "稍微" "再" "往前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "輕" "聲" "前" "進" "。"]))
  (it "can be reset"
    (jieba-reset)
    (expect
     (jieba-cut test:sample-1)
     :to-equal
     ["軌" "道" "逐" "漸" "往" "下" "，" "進" "到" "地下" "的" "區" "間" "。" "還" "沒" "有" "電" "車" "通" "過" "。" "稍微" "再" "往前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "輕" "聲" "前" "進" "。"]))
  (it "cuts all"
    (expect
     (jieba-cut-all test:sample-1)
     :to-equal
     ["道" "逐" "漸" "往" "往下" "下" "，" "進" "到" "地" "地下" "下" "的" "間" "。" "沒" "有" "電" "車" "通" "過" "。" "稍" "稍微" "微" "再" "再往" "往" "往前" "前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "前" "進" "。"]))
  (it "cuts for search"
    (expect
     (jieba-cut-for-search test:sample-1)
     :to-equal
     ["軌" "道" "逐" "漸" "往" "下" "，" "進" "到" "地下" "的" "區" "間" "。" "還" "沒" "有" "電" "車" "通" "過" "。" "稍微" "再" "往前" "會" "有" "一" "個" "站" "，" "我" "們" "朝" "著" "那" "裡" "輕" "聲" "前" "進" "。"])
    (expect
     (jieba-cut-for-search test:sample-1 t)
     :to-equal
     ["軌道" "逐漸" "往" "下" "，" "進到" "地下" "的" "區間" "。" "還沒有" "電車" "通過" "。" "稍微" "再" "往前" "會有" "一個" "站" "，" "我們" "朝著" "那裡" "輕聲" "前進" "。"]))
  (it "extracts keywords"
    (expect
     (jieba-extract-keywords test:sample-1 5)
     :to-equal
     '("稍微" "往前" "地下"))))

(buttercup-run)
