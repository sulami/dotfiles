{ pkgs ? (import <nixpkgs> {})
, stdenv ? pkgs.stdenv}:

let appName = "clj-kondo";
    version = "2020.11.07";
    platform = {
      "x86_64-linux" = "linux";
      "x86_64-darwin" = "macos";
    }."${stdenv.system}" or (throw "Unsupported platform");
    sha256 = {
      # TODO Get the other SHAs.
      "x86_64-linux" = "0000000000000000000000000000000000000000000000000000";
      "x86_64-darwin" = "046jkia2r53492gxmrjiqh7a3428drn9c3y8q36dlxpalbn55nmn";
    }."${stdenv.system}" or (throw "Unsupported platform");

in stdenv.mkDerivation {
  pname = appName;
  version = version;

  phases = ["installPhase"];

  src = pkgs.fetchzip {
    url = "https://github.com/borkdude/clj-kondo/releases/download/v${version}/clj-kondo-${version}-${platform}-amd64.zip";
    sha256 = "${sha256}";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp $src/${appName} $out/bin/${appName}
    chmod +x $out/bin/${appName}
  '';

  meta = {
    description = "";
    license = pkgs.lib.licenses.epl10;
    homepage = "https://github.com/borkdude/clj-kondo";
    platforms = [
      "x86_64-linux"
      "x86_64-darwin"
    ];
  };
}
