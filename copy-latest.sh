#!/usr/bin/env bash

# copies the existing configs in $HOME to this repo

items=( .bashrc .emacs .pinerc .pinerc-research .vimrc .zshrc .screenrc )

# check if there are uncommitted changes in the repo
if [[ -z $(git status -s) ]]
then
    # clobber each item in the current directory with the $HOME counterpart
    for item in "${items[@]}"
    do
        if [ -e $HOME/$item ]
        then
            cp -rvi $HOME/$item $item
        fi
    done
else
    echo ">> tree is dirty, please commit changes before running this"
    exit 1
fi
