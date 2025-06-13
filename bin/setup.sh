#!/bin/bash

mkdir -p ~/.emacs.d/external-modes

curl -o ~/.emacs.d/external-modes/clojure-mode.el \
  https://raw.githubusercontent.com/clojure-emacs/clojure-mode/master/clojure-mode.el

curl -o ~/.emacs.d/external-modes/go-mode.el \
  https://raw.githubusercontent.com/dominikh/go-mode.el/master/go-mode.el

curl -o ~/.emacs.d/external-modes/markdown-mode.el \
     https://raw.githubusercontent.com/jrblevin/markdown-mode/master/markdown-mode.el

curl -o ~/.emacs.d/external-modes/paredit.el \
     https://raw.githubusercontent.com/emacsmirror/paredit/master/paredit.el
