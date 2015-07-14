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
# if [[ $(uname) == "Linux" ]]; then
#     source /usr/share/doc/pkgfile/command-not-found.zsh
# fi

# Activate syntax highlighting
# source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Source my custom functions
for file in $HOME/dotfiles/zsh/functions/*.sh;
{
    source $file
}

# PROMPT
autoload -U colors && colors
RPS1="%(?..%{$fg[red]%}%?%{$reset_color%} <)"
# Adapt prompt when entering/leaving normal mode, and also color the
# prompt red if we are privileged. For this to work, this needs to be
# installed as global config.
function zle-line-init zle-keymap-select {
    PRMPT="${${KEYMAP/vicmd/N}/(main|viins)/λ}"
    PS1=" %(!.%{$fg[red]%}.)$PRMPT%{$reset_color%} "
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# OPTIONS
bindkey -v
setopt beep
setopt no_list_beep
setopt no_hist_beep
setopt no_flowcontrol
setopt autocd
setopt correct
setopt completealiases
setopt completeinword
setopt longlistjobs
setopt noglobdots
setopt noshwordsplit
setopt unset

# EXPORTS
if which nvim > /dev/null 2>&1; then
    # Enable neovim if it is installed.
    export EDITOR=nvim
    export VISUAL=nvim
else
    export EDITOR=vim
    export VISUAL=vim
fi
export BROWSER=firefox
export PATH=$PATH:$HOME/.local/bin:$HOME/.cabal/bin
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/usr/local/lib/pkgconfig
export GOPATH=$HOME/build/go
export GOMAXPROCS=8
export GITSERVER=pi@peerwire.dtdns.net
export GITURL=ssh://${GITSERVER}/srv/git

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
alias make='time make -j2'
alias py='ipython'
alias bp='bpython'
alias gc="$HOME/dotfiles/scripts/ghci-color"
alias psg='ps aux | grep'
alias gitsubup="git submodule foreach 'git pull origin master'"
alias gitauthors='git ls-tree -r -z --name-only HEAD -- * | xargs -0 -n1 git \
    blame --line-porcelain HEAD | grep  "^author " | sort | uniq -c | sort -nr'
alias gitsearch='git rev-list --all | pv | xargs git grep -F'
alias size='du -sh * | sort -rh'
alias rsync='rsync -aP --stats'
alias wget='wget -c'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias imgur='imgur-screenshot'
alias pc='sudo pacman'
alias em='sudo emerge --ask'
alias btrfs='sudo btrfs'

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
# Add incremental backwards search
bindkey '^r' history-incremental-search-backward
# Return to background program by hitting ^z again, thanks to grml
function zsh-fg() {
    if (( ${#jobstates} )); then
        zle .push-input
        [[ -o hist_ignore_space ]] && BUFFER=' ' || BUFFER=''
        BUFFER="${BUFFER}fg"
        zle .accept-line
    else
        zle -M 'No background jobs. Doing nothing.'
    fi
}
zle -N zsh-fg
bindkey '^z' zsh-fg
# fix not being able to delete past entry point (vim: set backspace)
bindkey -M viins '^h' backward-delete-char
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^w' backward-kill-word
# bind jk to exit insert mode, just like vim
bindkey -M viins 'jk' vi-cmd-mode

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
export MANPAGER='less'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export LESS=-r
export GROFF_NO_SGR=1

