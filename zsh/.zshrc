# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

# PROMPT
if [[ $(uname) != "OpenBSD" ]]; then
    # OpenBSD's zsh does not know about this
    prompt off
fi
autoload -U colors && colors

# git prompt stuff
source $HOME/dotfiles/zsh/zshrc.sh

PWD="%1~" # currently not in use
GITSTATUS='$(git_super_status)'
ERRORCODE="%(?..%{$fg[red]%}%?%{$reset_color%} < )"
RPS1="${ERRORCODE}${GITSTATUS}"
# Adapt prompt when entering/leaving normal mode
function zle-line-init zle-keymap-select {
    PS1=" ${${KEYMAP/vicmd/N}/(main|viins)/%#} "
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# Activate syntax highlighting
source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# OPTIONS
bindkey -v
unsetopt beep
unsetopt flowcontrol
setopt autocd
setopt correct
setopt completealiases
setopt completeinword
setopt longlistjobs
setopt noglobdots
setopt noshwordsplit
setopt unset

# EXPORTS
export PATH HOME TERM
export PKG_PATH=ftp://ftp.halifax.rwth-aachen.de/pub/OpenBSD/5.6/packages/amd64/
export ENV=$HOME/.kshrc
export EDITOR=nvim
export VISUAL=nvim
export BROWSER=firefox
export XDG_CONFIG_HOME="$HOME"
export PATH=$PATH:$HOME/.local/bin
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig
export GOPATH=$HOME/build/go
export GOMAXPROCS=8

# ALIASES
if [[ $(uname) == "Linux" ]]; then
    # GNU ls has coloring capabilities...
    alias ls='ls -F --color=auto'
    alias ll='ls -l --color=auto'
    alias la='ls -la --color=auto'
else
    # ...BSD ls not.
    alias ls='ls -F'
    alias ll='ls -l'
    alias la='ls -la'
fi
alias v='vim'
alias nv='nvim'
alias g='git'
alias make='time make -j16'
alias py='ipython'
alias python='python2.7'
alias psg='ps aux | grep'
alias gitup='git fetch && git co origin/master && git st'
alias gitpush='git pom && git pgm && git plm'
alias gitsubup="git submodule foreach 'git pull origin master'"
alias gitauthors='git ls-tree -r -z --name-only HEAD -- * | xargs -0 -n1 git \
    blame --line-porcelain HEAD | grep  "^author " | sort | uniq -c | sort -nr'
alias gitsearch='git rev-list --all | pv | xargs git grep -F'
alias size='du -sh * | pv | sort -rh'
alias rsync='rsync -aP --stats'
alias wget='wget -c'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias imgur='imgur-screenshot'
alias news='newsbeuter'
alias yum='sudo yum'
alias pc='sudo pacman'
alias btrfs='sudo btrfs'

# COMPLETIONS
compdef gpg2=gpg

# KEYBINDS
typeset -A key
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char
bindkey '\E[1;5D' backward-word
bindkey '\E[1;5C' forward-word
# fix not being able to delete past entry point (vim: set backspace)
bindkey -M viins '^h' backward-delete-char
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^w' backward-kill-word
# bind jk to exit insert mode, just like vim
bindkey -M viins 'jk' vi-cmd-mode

# Source my custom functions
for file in $HOME/dotfiles/zsh/functions/*.sh;
{
    source $file
}

# X-less colours
if [ "$TERM" = "linux" ]; then
    echo -en "\e]P0121212" #black
    echo -en "\e]P83B3B3B" #darkgrey
    echo -en "\e]P1CF6A4C" #darkred
    echo -en "\e]P9CF6A4C" #red
    echo -en "\e]P299AD6A" #darkgreen
    echo -en "\e]PA99AD6A" #green
    echo -en "\e]P3D8AD4C" #brown
    echo -en "\e]PBD8AD4C" #yellow
    echo -en "\e]P4579BC5" #darkblue
    echo -en "\e]PC579BC5" #blue
    echo -en "\e]P5A037B0" #darkmagenta
    echo -en "\e]PDA037B0" #magenta
    echo -en "\e]P671B9F8" #darkcyan
    echo -en "\e]PE71B9F8" #cyan
    echo -en "\e]P7ADADAD" #lightgrey
    echo -en "\e]PFADADAD" #white
    clear #for background artifacting
fi

# Colored manpages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export LESS=-r
export GROFF_NO_SGR=1

