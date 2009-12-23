#!/bin/sh

pacman -S gpg sbcl vim vim-colorsamplerpack tidy screen

cp common-lisp-net-keyring.asc /tmp/keyring.asc
#wget http://www.common-lisp.net/keyring.asc -O /tmp/keyring.asc
#gpg --import keyring.asc
rm -f /tmp/keyring.asc

echo "(require 'asdf-install)
      (asdf-install:install 'hunchentoot)
      (asdf-install:install 'html-template)
      (asdf-install:install 'linedit)" | sbcl --script

DIR=`pwd`
cd /tmp
tar xvzf $DIR/HyperSpec-7-0.tar.gz
mv /tmp/HyperSpec /usr/share/doc/hyperspec
rm -rf /tmp/HyperSpec*
tar xvzf $DIR/limp-0.3.4-arno.tar.gz
cd limp-0.3.4
./install.sh
rm -rf /tmp/limp-0.3.4*
