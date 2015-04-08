# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

# Zsh does not load .profile like ksh does (at least not by default w/o
# any compability mode, but we need it for login shells on OpenBSD.
if [[ $(uname) == "OpenBSD" && -o login ]]; then
    source $HOME/.profile
fi

source "$HOME/dotfiles/zsh/zshrc"

# Activate syntax highlighting
# source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Source my custom functions
for file in $HOME/dotfiles/zsh/functions/*.sh;
{
    source $file
}

