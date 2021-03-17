rec {
  username = "sulami";
  hostname = "wintermute";
  computer_name = "Wintermute";
  home_directory = "/Users/${username}";
  nix_config = {
    allowUnfree = true;
  };
}
