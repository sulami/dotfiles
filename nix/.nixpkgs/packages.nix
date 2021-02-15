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
  grpcurl
  isync
  iterm2
  jq
  leiningen
  miller
  msmtp
  mtr
  netcat
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
  stack
  stow
  tmux
  tree
  unrar
  vim
  watch
  wget
  zsh
] ++ [
  (import ./packages/clj-kondo.nix {})
  (import ./packages/titlecase.nix {})
  (import ./packages/zprint.nix {})
]
