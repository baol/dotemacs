# baol's dotemacs

My personal .emacs for C++, html, and partially python and ruby.

## installing

First fetch the submodules

    git submodule init
    git submodule update

Then installing on unices is just a matter of

    ln -s doteamcs ~/.emacs.d

All packages except the most important one will be automatically
installed.  To install the great
[rtags](https://github.com/Andersbakken/rtags), follow the link and
the instructions on the related site.

You can install rtags from source, remember to make install as
well (and of course to create the index for your project).

## other dependencies

On Ubuntu you'll need some other packages to make it work:

    sudo apt-get install silversearcher-ag markdown
       python-virtualenv clang-format-3.6

## note

This repository is here for my convenience (setting up emacs quickly
between machines) but it will turn your emacs into a powerful
development environment for C++ as well!

I'm trying to balance functionalities and start time, a nice trick is
to add the following alias to your shell to start the client or the
server automatically:

    alias emacs='emacsclient -a emacs -c "$@"'

On MacOS X it is slightly more convoluted:

    alias emacs='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -a /Applications/Emacs.app/Contents/MacOS/Emacs -c "$@"'

> -baol
