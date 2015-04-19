# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

# Zsh does not load .profile like ksh does (at least not by default w/o
# any compability mode, but we need it for login shells on OpenBSD.
if [[ $(uname) == "OpenBSD" && -o login && -z $TMUX ]]; then
    source $HOME/.profile
fi

# If on Arch, source the pkgfile command-not-found script that tells us
# where to find a binary that is not installed.
if [[ $(uname) == "Linux" ]]; then
    source /usr/share/doc/pkgfile/command-not-found.zsh
fi

source "$HOME/dotfiles/zsh/zshrc"

# Activate syntax highlighting
# source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Source my custom functions
for file in $HOME/dotfiles/zsh/functions/*.sh;
{
    source $file
}

