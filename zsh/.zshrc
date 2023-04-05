# BASIC STUFF
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
autoload -U colors && colors
autoload -Uz compinit && compinit -i

# Activate syntax highlighting
if [[ "dumb" != "$TERM" ]]; then
    source "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi

if which emacs > /dev/null 2>&1; then
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

# M-x shell
if [[ "dumb" == "$TERM" ]]; then
    export PAGER='cat'
fi

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
alias ls='exa -F'
alias ll='ls -l'
alias la='ll -a'
alias g='git $*'
compdef g='git'
alias psg='ps aux | grep $*'
alias rsync='rsync -aP --stats $*'
alias dps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'
alias dc='docker compose $*'
alias dcud='docker-compose up -d $*'
alias dclf='docker-compose logs --tail=10 -f $*'

# Copy aliases over to eshell
alias | gsed 's/^alias //' | gsed -E "s/^([^=]+)='(.+?)'$/\1=\2/" | gsed "s/'\\\\''/'/g" | gsed "s/'\\\\$/'/;" | gsed -E 's/^([^=]+)=(.+)$/alias \1 \2/' > ~/.emacs.d/aliases
echo 'alias ff find-file $1' >> ~/.emacs.d/aliases
echo 'alias ffw find-file-other-window $1' >> ~/.emacs.d/aliases

# Makes a new dir and cds to it.
mcd()
{
    mkdir -p "$1" && cd "$1"
}

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

source /opt/homebrew/opt/asdf/libexec/asdf.sh
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
export PATH="/opt/homebrew/opt/mysql-client@5.7/bin:$PATH"
