{ pkgs, home_directory, ... }:

{
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
}
