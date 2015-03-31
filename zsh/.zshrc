# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

source "$HOME/dotfiles/zsh/zshrc"

# Activate syntax highlighting
source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Source my custom functions
for file in $HOME/dotfiles/zsh/functions/*.sh;
{
    source $file
}

