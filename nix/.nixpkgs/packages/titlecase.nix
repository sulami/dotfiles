{ stdenv ? (import <nixpkgs> {}).stdenv
, fetchurl ? (import <nixpkgs> {}).fetchurl }:

let appName = "titlecase";
    version = "1.1.0";
    platform_mapping = {
      "x86_64-linux" = "x86_64-unknown-linux";
      "x86_64-darwin" = "x86_64-apple-darwin";
      "x86_64-freebsd" = "x86_64-unknown-freebsd";
      "x86_64-netbsd" = "x86_64-unknown-netbsd";
      "x86_64-openbsd" = "x86_64-unknown-openbsd";
    };

in stdenv.mkDerivation {
  pname = appName;
  version = version;

  src = fetchurl {
    url = "https://releases.wezm.net/${appName}/${version}/${appName}-${version}-${platform_mapping.${builtins.currentSystem}}.tar.gz";
    sha256 = "12cs13wsz58k0wijs0gxd8h8dg7cwysjc1cbn347v1z2chjgkxxf";
  };

  setSourceRoot = "sourceRoot=`pwd`";

  installPhase = ''
    mkdir -p $out/bin
    cp ${appName} $out/bin/${appName}
    chmod +x $out/bin/${appName}
  '';

  meta = {
    description = "A tool and Rust crate for transforming text into Title Case";
    license = stdenv.lib.licenses.mit;
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
