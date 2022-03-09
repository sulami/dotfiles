pkgs:

with pkgs;
[
  # clj-kondo
  adoptopenjdk-hotspot-bin-8
  arduino-cli
  aspell
  aspellDicts.en
  avrdude
  circleci-cli
  clojure
  clojure-lsp
  cmake
  docker
  docker-compose
  emacsMacport
  entr
  fd
  git
  gnupg
  gnused
  go_1_17
  gopls
  grpcurl
  isync
  jq
  leiningen
  miller
  msmtp
  mtr
  ngrok
  nodePackages.typescript
  nodePackages.typescript-language-server
  notmuch
  pandoc
  pass
  pkgs.pkgsCross.avr.buildPackages.gcc
  pinentry_mac
  poetry
  pv
  restic
  ripgrep
  rlwrap
  ruby
  rustup
  skhd
  slack
  socat
  stack
  stow
  tokei
  tree
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
