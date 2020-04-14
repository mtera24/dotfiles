cd ..\.emacs.d
mklink init.el "..\dotfiles\.emacs.d\init.el"
cd "..\dotfiles"
rem
rem dotfilesディレクトリから、
rem mklink ..\.emacs.d\init.el .emacs.d\init.el
rem では、逆リンクになるので貼れなかった。
