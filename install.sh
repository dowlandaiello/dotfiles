#! /usr/bin/env sh

echo "== Checking for capnproto linting emacs support ==" && stat ~/emacspkg/capnproto/highlighting/emacs || (echo "\nInstalling emacs dependencies." && mkdir -p ~/emacspkg/capnproto/highlighting/emacs && curl https://raw.githubusercontent.com/capnproto/capnproto/master/highlighting/emacs/capnp-mode.el -o ~/emacspkg/capnproto/highlighting/emacs/capnp-mode.el && echo "\nDone!")
