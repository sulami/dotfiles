# M-x shell
if [[ "dumb" == "$TERM" ]]; then
    export PAGER='cat'
fi

# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
zstyle :compinstall filename '$HOME/.zshrc'
fpath=(~/.zsh/completion $fpath)
autoload -Uz compinit
compinit -i

if which emacs > /dev/null 2>&1; then
    export VISUAL="$(which emacsclient) -c -a $(which emacs)"
    export EDITOR="$VISUAL"
elif which nvim > /dev/null 2>&1; then
    export EDITOR=nvim
    export VISUAL=nvim
elif which vim > /dev/null 2>&1; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=vi
    export VISUAL=vi
fi
export LC_ALL=en_US.UTF-8

# Git prompt import
#source $HOME/dotfiles/zsh/zsh-git-prompt/zshrc.sh
#export GIT_PROMPT_EXECUTABLE="haskell"

export GOMAXPROCS=8

PYTHON_3_VERSION=$(python3 -c "import sys; print('{}.{}'.format(sys.version_info.major, sys.version_info.minor))")
export PATH=$PATH:$HOME/Library/Python/${PYTHON_3_VERSION}/bin
# Virtualenvwrapper support if available
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
if which virtualenvwrapper_lazy.sh > /dev/null 2>&1; then
    source "$(which virtualenvwrapper_lazy.sh)"
fi

export LEIN_FAST_TRAMPOLINE=y

# Activate syntax highlighting
source "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# PROMPT
autoload -U colors && colors
RPS1="%(?..%{$fg_bold[red]%}%?%{$reset_color%} <)"
# Adapt prompt when entering/leaving normal mode, and also color the
# prompt red if we are privileged. For this to work, this needs to be
# installed as global config.
function zle-line-init zle-keymap-select {
    # hostname:pwd(git status)
    PPREFIX="%{$fg_bold[magenta]%}%1~%{$reset_color%}"
    # Mode-dependent symbol
    PRMPT="${${KEYMAP/vicmd/N}/(main|viins)/λ}"
    # Put it all together, color the symbol if there are bg jobs
    PS1="$PPREFIX %1(j.%{$fg[cyan]%}$PRMPT%{$reset_color%}.$PRMPT) "
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# OPTIONS
bindkey -v
setopt beep
setopt no_listbeep
setopt no_histbeep
setopt no_flowcontrol
setopt autocd
setopt correct
setopt completealiases
setopt completeinword
setopt longlistjobs
setopt noglobdots
setopt noshwordsplit
setopt unset

# Often used options
alias v='$EDITOR $*'
alias g='git $*'
compdef g='git'
alias make='time make -j2 $*'
alias psg='ps aux | grep $*'
alias rsync='rsync -aP --stats $*'
alias wget='wget -c $*'
alias dps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'
alias drun='docker run -it $*'
alias dc='docker-compose $*'
alias dcud='docker-compose up --build -d'
alias dcd='docker-compose down -v --remove-orphans'
alias dclf='docker-compose logs --tail=10 -f $*'

# Copy aliases over to eshell
alias | gsed 's/^alias //' | gsed -E "s/^([^=]+)='(.+?)'$/\1=\2/" | gsed "s/'\\\\''/'/g" | gsed "s/'\\\\$/'/;" | gsed -E 's/^([^=]+)=(.+)$/alias \1 \2/' > ~/.emacs/aliases

# Starts up a new emacs session and decouples it from the shell.
function edit() {
    emacs $* > /dev/null & ; disown
}

# Makes a new dir and cds to it.
mcd()
{
    mkdir -p "$1" && cd "$1"
}

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
#bindkey -M viins 'jk' vi-cmd-mode
# bind hmenu
# bindkey -s '^h' 'hmenu\n'

# Set the colourscheme according to the time of day
#if [[ -z "$TMUX" ]]; then
#  HOUR="$(date +'%H')"
#  if [[ 6 -lt $HOUR && $HOUR -lt 20 ]]; then
#    $HOME/.dynamic-colors/bin/dynamic-colors switch solarized-light
#  else
#    $HOME/.dynamic-colors/bin/dynamic-colors switch solarized-dark
#  fi
#fi

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

# Evaluate system PATH
if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi
