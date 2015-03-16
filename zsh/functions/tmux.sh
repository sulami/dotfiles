#!/bin/zsh

# This is figuring out if we are in tmux, if not tries to attach to a
# running tmux session and if that fails, opens up a new session and
# executes $HOME/.profile, which is supposed to print out my welcome
# screen.
tmux_auto()
{
    test -z "$TMUX" && (tmux attach || tmux new 'sh $HOME/.profile && zsh')
}

