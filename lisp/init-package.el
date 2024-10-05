(require 'package)
(setq package-archives '
    (
        ("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

;;防止反复调用package-refresh-contents会影响加载速度
(when
    (not package-archive-contents)
  (package-refresh-contents))
(package-install 'use-package)

(provide 'init-package)
