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

if [ -f /var/run/current-system/Applications/Emacs.app/Contents/MacOS/Emacs ]; then
    export EDITOR=/var/run/current-system/Applications/Emacs.app/Contents/MacOS/Emacs
elif which emacs > /dev/null 2>&1; then
    export EDITOR="$(which emacsclient) -c -a $(which emacs)"
elif which nvim > /dev/null 2>&1; then
    export EDITOR=nvim
elif which vim > /dev/null 2>&1; then
    export EDITOR=vim
else
    export EDITOR=vi
fi
export VISUAL="$EDITOR"
export LC_ALL=en_US.UTF-8
export GOMAXPROCS=8
export LEIN_FAST_TRAMPOLINE=y

# Activate syntax highlighting
if [[ "dumb" != "$TERM" ]]; then
    source "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi

# PROMPT
autoload -U colors && colors
# This was a right-side non-zero return code.
# RPS1="%(?..%{$fg_bold[red]%}%?%{$reset_color%} <)"
RPS1=""
# Adapt prompt when entering/leaving normal mode, and also color the
# prompt red if we are privileged. For this to work, this needs to be
# installed as global config.
function zle-line-init zle-keymap-select {
    PPREFIX="%(?..<%{$fg_bold[red]%}%?%{$reset_color%}> )%{$fg_bold[magenta]%}%1~%{$reset_color%}"
    # Mode-dependent symbol
    PRMPT="${${KEYMAP/vicmd/N}/(main|viins)/λ}"
    # Put it all together, color the symbol if there are bg jobs
    PS1="$PPREFIX %1(j.%{$fg[cyan]%}$PRMPT%{$reset_color%}.$PRMPT) "
    zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select

# OPTIONS
bindkey -e
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
setopt sharehistory

# Often used options
alias e='$EDITOR $*'
alias v='$EDITOR $*'
alias em='open -na /run/current-system/Applications/Emacs.app --args --chdir=$(pwd) $*'
alias g='git $*'
compdef g='git'
alias make='time make -j2 $*'
alias psg='ps aux | grep $*'
alias rsync='rsync -aP --stats $*'
alias wget='wget -c $*'
alias dps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'
alias dc='docker-compose $*'
compdef dc='docker-compose'
alias dcud='docker-compose up -d $*'
alias dclf='docker-compose logs --tail=10 -f $*'
alias pause_docker="docker ps | awk '/Up/ {print \$1}' | xargs docker pause"
alias unpause_docker="docker ps | awk '/(Paused)/ {print \$1}' | xargs docker unpause"

# Copy aliases over to eshell
alias | gsed 's/^alias //' | gsed -E "s/^([^=]+)='(.+?)'$/\1=\2/" | gsed "s/'\\\\''/'/g" | gsed "s/'\\\\$/'/;" | gsed -E 's/^([^=]+)=(.+)$/alias \1 \2/' > ~/.emacs/aliases
echo 'alias ff find-file $1' >> ~/.emacs/aliases
echo 'alias ffw find-file-other-window $1' >> ~/.emacs/aliases

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

# GPG Agent
if test -e "$(gpgconf --list-dirs agent-ssh-socket)" -a -n "$(pgrep gpg-agent)"; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
else
    eval $(gpg-agent --daemon)
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

if [ -e /Users/sulami/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/sulami/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

if [ -e /etc/static/zshrc ]; then . /etc/static/zshrc; fi # Nix-darwin
