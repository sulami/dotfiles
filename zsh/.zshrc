# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

# PROMPT
autoload -U colors && colors
source $HOME/dotfiles/zsh/zshrc.sh
# PROMPT="%{$fg[green]%} %# %{$reset_color%}"
# PROMPT="%B%1~%b$(git_super_status) %B%#%b "
# PROMPT="%B%1~ %#%b "
PROMPT='%1~$(git_super_status) %# '
ERRORCODE="%(?..%{$fg[red]%} %? <<%{$reset_color%})"
RPROMPT="${ERRORCODE}"

# OPTIONS
setopt autocd
unsetopt beep
setopt correct
bindkey -e
unsetopt flowcontrol

# VARIABLES
export EDITOR=vim
export VISUAL=vim
export XDG_CONFIG_HOME="$HOME"

# ALIASES
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -la --color=auto'
alias v='vim'
alias g='git'
alias make='time make -j16'
alias py='ipython2'
alias py3='ipython3'
alias psg='ps aux | grep'
alias gitup='git fetch && git co origin/master && git st'
alias gitpush='git pom && git pgm && git plm'
alias gitauthors='git ls-tree -r -z --name-only HEAD -- * | xargs -0 -n1 git \
    blame --line-porcelain HEAD | grep  "^author " | sort | uniq -c | sort -nr'
alias gitsearch='git rev-list --all | pv | xargs git grep -F'
alias size='du -sh * | pv | sort -rh'
alias rsync='rsync -aP --stats'
alias wget='wget -c'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
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

# Activate syntax highlighting
source "$HOME/dotfiles/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Source my custom functions
source $HOME/dotfiles/zsh/functions/*.sh

# X colours
xrdb -merge ~/dotfiles/Xresources/jellybeans

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

