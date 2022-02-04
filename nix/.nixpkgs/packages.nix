pkgs:

with pkgs;
[
  # clj-kondo
  adoptopenjdk-hotspot-bin-8
  aspell
  aspellDicts.en
  circleci-cli
  cloc
  clojure
  clojure-lsp
  cmake
  docker
  docker-compose
  emacsMacport
  fd
  fira-code
  git
  gnupg
  gnused
  go_1_17
  gopls
  grpcurl
  isync
  iterm2
  jq
  leiningen
  miller
  msmtp
  mtr
  ngrok
  nodePackages.typescript
  notmuch
  oil
  pandoc
  pass
  pinentry_mac
  poetry
  pv
  racket-minimal
  restic
  ripgrep
  rlwrap
  ruby
  sbcl
  skhd
  slack
  socat
  stack
  stow
  tmux
  tree
  unrar
  vim
  watch
  xh
  yq
  zsh
] ++ [
  (import ./packages/babashka.nix { pkgs = pkgs; })
  (import ./packages/clj-kondo.nix { pkgs = pkgs; })
  # (import ./packages/esp-rs.nix { pkgs = pkgs; })
  (import ./packages/titlecase.nix { pkgs = pkgs; })
  (import ./packages/zprint.nix { pkgs = pkgs; })
]
