{ pkgs ? (import <nixpkgs> {})
, stdenv ? pkgs.stdenv}:

let appName = "titlecase";
    version = "1.1.0";
    platform = {
      "x86_64-linux" = "x86_64-unknown-linux";
      "x86_64-darwin" = "x86_64-apple-darwin";
      "x86_64-freebsd" = "x86_64-unknown-freebsd";
      "x86_64-netbsd" = "x86_64-unknown-netbsd";
      "x86_64-openbsd" = "x86_64-unknown-openbsd";
    }."${stdenv.system}" or (throw "Unsupported platform");
    sha256 = {
      # TODO Get the other SHAs.
      "x86_64-linux" = "0000000000000000000000000000000000000000000000000000";
      "x86_64-darwin" = "12cs13wsz58k0wijs0gxd8h8dg7cwysjc1cbn347v1z2chjgkxxf";
      "x86_64-freebsd" = "0000000000000000000000000000000000000000000000000000";
      "x86_64-netbsd" = "0000000000000000000000000000000000000000000000000000";
      "x86_64-openbsd" = "0000000000000000000000000000000000000000000000000000";
    }."${stdenv.system}" or (throw "Unsupported platform");

in stdenv.mkDerivation {
  pname = appName;
  version = version;

  src = pkgs.fetchurl {
    url = "https://releases.wezm.net/${appName}/${version}/${appName}-${version}-${platform}.tar.gz";
    sha256 = "${sha256}";
  };

  setSourceRoot = "sourceRoot=`pwd`";

  installPhase = ''
    mkdir -p $out/bin
    cp ${appName} $out/bin/${appName}
    chmod +x $out/bin/${appName}
  '';

  meta = {
    description = "A tool and Rust crate for transforming text into Title Case";
    license = pkgs.lib.licenses.mit;
    homepage = "https://github.com/wezm/titlecase";
    platforms = [
      "x86_64-linux"
      "x86_64-darwin"
      "x86_64-freebsd"
      "x86_64-netbsd"
      "x86_64-openbsd"
    ];
  };
}
