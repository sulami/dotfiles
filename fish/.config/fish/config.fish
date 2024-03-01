if status is-interactive
    # Commands to run in interactive sessions can go here
    set -gx EDITOR nvim
    set -gx VISUAL $EDITOR

    fish_add_path ~/.cargo/bin
    fish_add_path ~/.asdf/shims
    fish_add_path /opt/homebrew/bin
    fish_add_path /opt/homebrew/sbin

    alias ls='exa -F'
    alias ll='ls -l'
    alias la='ll -a'
    alias g='git'
    alias dc='docker-compose'

    function psg
      ps aux | grep $argv
    end

    function mcd
      mkdir -p $argv | cd $argv
    end

    eval "$(zoxide init fish)"

    # GPG Agent
    if test -e "$(gpgconf --list-dirs agent-ssh-socket)" -a -n "$(pgrep gpg-agent)"
        set -gx SSH_AUTH_SOCK "$(gpgconf --list-dirs agent-ssh-socket)"
    else
        eval $(gpg-agent --daemon)
        set -gx SSH_AUTH_SOCK "$(gpgconf --list-dirs agent-ssh-socket)"
    end
end
