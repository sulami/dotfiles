if status is-interactive
  # Commands to run in interactive sessions can go here
  set -gx EDITOR nvim
  set -gx VISUAL $EDITOR

  fish_add_path ~/.cargo/bin
  fish_add_path ~/.local/bin
  fish_add_path /opt/homebrew/bin
  fish_add_path /opt/homebrew/sbin

  alias ls='eza -F'
  alias ll='ls -l'
  alias la='ll -a'
  alias g='git'
  alias dc='docker-compose'
  alias tf='terraform'
  alias uuid="uuidgen | tr '[:upper:]' '[:lower:]' | pbcopy"

  function psg
    ps aux | grep $argv
  end

  function mcd
    mkdir -p $argv && cd $argv
  end

  # Mise
  mise activate fish | source

  # Zoxide
  eval "$(zoxide init fish)"

  # SSH through Bitwarden
  if test -S "$HOME/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
    set -gx SSH_AUTH_SOCK "$HOME/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
  end

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
set --export --prepend PATH "/Users/sulami/.rd/bin"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

end
