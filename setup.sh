#!/bin/sh

set +e

setup_dir=`dirname $(realpath $0)`
local_bin_dir="$HOME/.local/bin"

main() {
    link_files

    mkdir -p ~/.emacs.d/external-modes

    ensure_mode ~/.emacs.d/external-modes/clojure-mode.el \
		clojure-emacs/clojure-mode/master/clojure-mode.el

    ensure_mode ~/.emacs.d/external-modes/go-mode.el \
		dominikh/go-mode.el/master/go-mode.el

    ensure_mode ~/.emacs.d/external-modes/markdown-mode.el \
		jrblevin/markdown-mode/master/markdown-mode.el

    ensure_mode ~/.emacs.d/external-modes/paredit.el \
		emacsmirror/paredit/master/paredit.el
}

ensure_mode() {
    [[ ! -f $1 ]] && curl -o $1 "https://raw.githubusercontent.com/$2"
}

link_files() {
    mkdir -p ~/.local/bin
    for f in $setup_dir/bin/*; do
        ln -sfv $f $local_bin_dir/`basename $f`
    done
}

main
