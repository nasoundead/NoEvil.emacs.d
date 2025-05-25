(use-package anzu
 :ensure t
  ;;   :init (global-anzu-mode +1)
 :hook
 (after-init . global-anzu-mode)
 :bind (([remap query-replace] . anzu-query-replace)
       ([remap query-replace-regexp] . anzu-query-replace-regexp)
       :map isearch-mode-map
       ([remap isearch-query-replace] . anzu-isearch-query-replace)
       ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
 :config (setq anzu-replace-to-string-separator
         (if (char-displayable-p ?→) " → " " -> ")))

(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "%s/%s ")
(setq lazy-highlight-cleanup nil)
;; 这样可以在literal的isearch中，把空格直接当成正则里面的.*匹
(setq isearch-lax-whitespace t)
(setq search-whitespace-regexp ".*")
;; 默认的isearch-forward函数是literal的，也就是用户输入什么就匹配什么，没有正则解释，没有转义，完全literal。这样的好处就是，想搜啥就是啥，不用考虑太多。其实默认用它就可以了。
;; 当然，完全可以开启正则匹配等功能，下面就说说这几个toggle函数。
;; isearch-toggle-regexp 在使用isearch搜索时（即按下C-s isearch-forward后）绑定到M-s r。 按下后，您的输入全部都会被以正则来匹配了。
;; isearch-toggle-case-fold 默认绑到M-s c。默认isearch对大小写是类似于rg一样“smart”的。具体地说，如果用户全部输入小写，则不匹分大小写进行匹配，如果用户输入中包括大写，则精确匹配大小写。 再举个例子，默认情况下，默认foo可以匹配foo,Foo,FOO。输入Foo，只能匹配到Foo。打开这个选项后，就是case sensitive了，也就只能精确匹配了。个人认为，该选项用处不太大。
;; isearch-toggle-word 默认绑定到M-s w。打开word匹配。直接举例：未打开以前，foo可以匹配foobar，foo。打开该选项后，foo只能匹配foo了，foobar就匹配不到了。 可以看出来，开启该选项后，isearch必须完全匹配一个完整地word。这个功能可以帮忙过滤很多杂项。
;; isearch-toggle-symbol 默认绑定到M-s _ 。它和isearch-toggle-word的基本一样，不过它使isearch完全匹配一个symbol。 简单来说，symbol和word的区别：isearch-toggle-word是一个symbol，它包括isearch toggle和word三个word。
;; isearch-toggle-lax-whitespace 默认绑定到M-s SPC。开启该功能后，可以把输入中的空格当做一个固定的正则表达式，这个固定的正则表达式存在于search-whitespace-regexp变量中。关于这个功能，我在后面 空格的特殊用法 中进行详细说明。
(with-eval-after-load 'isearch
 ;; DEL during isearch should edit the search string, not jump back to the previous result
 (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
 ;; Activate occur easily inside isearch
 (when (fboundp 'isearch-occur)
 ;; to match ivy conventions
 (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur))

  ;;   (define-key isearch-mode-map (kbd "<C-return>") 'swiper-from-isearch)

 (defadvice isearch-search (after isearch-no-fail activate)
   (unless isearch-success
  (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
  (ad-activate 'isearch-search)
  (isearch-repeat (if isearch-forward 'forward))
  (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
  (ad-activate 'isearch-search))))

(provide 'init-search)
