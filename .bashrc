#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1="\e[1;032m\w > \$ \e[0;033m"
EDITOR=vim
VISUAL=vim
