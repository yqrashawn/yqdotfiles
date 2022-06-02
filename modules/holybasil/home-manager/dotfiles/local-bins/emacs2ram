#!/bin/sh

if [ -z "$1" ];then
    echo "Usage:"
    echo "  emacs2ram start"
    echo "  emacs2ram restore"
    exit 1
fi

if [ "$1" == "start" ];then
    backup=emacs.d-backup
    link=.emacs.d
    volatile=/tmp/.emacs.d-$USER

    IFS=
    set -efu

    cd ~/

    if [ ! -r $volatile ]; then
        mkdir -m0700 $volatile
    fi

    # link -> volatie does not exist
    if [ "$(readlink $link)" != "$volatile" ]; then
        # backup project at first
        mv $link $backup
        # create the link
        ln -s $volatile $link
    fi

    if [ -e $link/.unpacked ]; then
        echo "Sync .emacs.d from memory to backup ..."
        rsync -avq --delete --exclude .unpacked ./$link/ ./$backup/
        echo "DONE!"
    else
        echo "Sync .emacs.d from disk to memory ..."
        rsync -avq ./$backup/ ./$link/
        touch $link/.unpacked
        echo "DONE!"
    fi
else
    echo "Moving .emacs.d back to disk ..."
    backup=$2-backup
    link=$2
    volatile=/tmp/$2-$USER
    cd ~/projs
    rm $link && mv $backup $link && rm -rf $volatile
    echo "DONE!"
fi
