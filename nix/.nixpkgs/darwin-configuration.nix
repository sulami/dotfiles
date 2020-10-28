{ config, pkgs, ... }:

let home_directory = "/Users/sulami";
in {
  imports = [ <home-manager/nix-darwin> ];

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;

  users.users.sulami.name = "sulami";
  users.users.sulami.home = home_directory;

  networking.computerName = "Shodan";
  networking.hostName = "Shodan";

  system.keyboard = {
    enableKeyMapping = true;
    userKeyMapping = [
      {
        # Capslock -> Ctrl
        HIDKeyboardModifierMappingSrc = 30064771129;
        HIDKeyboardModifierMappingDst = 30064771296;
      }
      {
        # Ctrl -> Escape
        HIDKeyboardModifierMappingDst = 30064771113;
        HIDKeyboardModifierMappingSrc = 30064771129;
      }
    ];
  };

  system.defaults.NSGlobalDomain = {
    AppleMeasurementUnits                = "Centimeters";
    AppleMetricUnits                     = 1;
    AppleTemperatureUnit                 = "Celsius";
    AppleShowScrollBars                  = "Automatic";
    InitialKeyRepeat                     = 12;
    KeyRepeat                            = 2;
    NSAutomaticCapitalizationEnabled     = false;
    NSAutomaticPeriodSubstitutionEnabled = false;
  };

  system.defaults.dock = {
    autohide            = true;
    expose-group-by-app = false;
    mru-spaces          = false;
    tilesize            = 128;
  };

  system.defaults.spaces.spans-displays = false;

  system.defaults.trackpad = {
    Clicking                = false;
    TrackpadRightClick      = true;
  };

  launchd.user.agents = {
    isync = {
      command = "${pkgs.isync}/bin/mbsync -a";
      serviceConfig = {
        ProcessType = "Background";
        LowPriorityIO = true;
        StartInterval = 5 * 60;
        RunAtLoad = true;
        KeepAlive = false;
        StandardErrorPath = "${home_directory}/Desktop/MAIL_STDERR";
        EnvironmentVariables = {
          "PATH" = "${pkgs.pass}/bin:$PATH";
          "SSL_CERT_FILE" = "/etc/ssl/certs/ca-certificates.crt";
        };
      };
    };

    skhd = {
      command = "${pkgs.skhd}/bin/skhd";
      serviceConfig = {
        RunAtLoad = true;
        KeepAlive = true;
      };
    };

    backup = {
      command = "${home_directory}/dotfiles/restic/.restic/backup.rb";
      serviceConfig = {
        ProcessType = "Background";
        LowPriorityIO = true;
        StartInterval = 24 * 60 * 60;
        RunAtLoad = false;
        StandardErrorPath = "${home_directory}/Desktop/BACKUP_STDERR";
      };
    };
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;

  };

  # TODO Split this up.
  # TODO Move this into dotfiles.

  home-manager = {
    users.sulami = { pkgs, ... }: {
      nixpkgs.config.allowUnfree = true;

      home.packages = with pkgs; [
        adoptopenjdk-jre-hotspot-bin-8
        aspell
        aspellDicts.en
        circleci-cli
        # clj-kondo
        leiningen
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
        msmtp
        mtr
        netcat
        ngrok
        notmuch
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
        nodePackages.typescript
        unrar
        vim
        watch
        wget
        zsh
      ];
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
