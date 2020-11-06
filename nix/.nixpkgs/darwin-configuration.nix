{ config, pkgs, ... }:

let constants = import ./constants.nix;
    darwin_config = import ./darwin.nix;
in {
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = import ./packages.nix pkgs;

  environment.variables = {
    JAVA_HOME = "/run/current-system/sw";
  };

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;

  users.users.sulami.name = constants.username;
  users.users.sulami.home = constants.home_directory;

  networking.computerName = constants.computer_name;
  networking.hostName = constants.hostname;

  launchd.user.agents = import ./launchd.nix {
    pkgs = pkgs;
    home_directory = constants.home_directory;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
} // darwin_config
