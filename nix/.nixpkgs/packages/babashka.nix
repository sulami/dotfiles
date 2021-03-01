{ pkgs ? (import <nixpkgs> {})
, stdenv ? pkgs.stdenv }:

let appName = "babashka";
    binary = "bb";
    version = "0.2.12";
    platform = {
      "x86_64-linux" = "linux-static-amd64";
      "x86_64-darwin" = "macos-amd64";
    }."${stdenv.system}" or (throw "Unsupported platform");
    sha256 = {
      "x86_64-linux" = "1ipbsxl8hfd87sdqs4p71v81l65ws2gcyvax7vw3yzyvc6v2lb7i";
      "x86_64-darwin" = "1zkx027p7bj0wbmx6abv7xq0w2gly9pxjyiy9zbvnpwk2ay1ybgr";
    }."${stdenv.system}" or (throw "Unsupported platform");

in stdenv.mkDerivation {
  pname = appName;
  version = version;

  phases = ["installPhase"];

  src = pkgs.fetchzip {
    url = "https://github.com/babashka/${appName}/releases/download/v${version}/${appName}-${version}-${platform}.zip";
    sha256 = "${sha256}";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp $src/${binary} $out/bin/${binary}
    chmod +x $out/bin/${binary}
  '';

  meta = {
    description = "Native Clojure interpreter for scripting";
    license = pkgs.lib.licenses.epl10;
    homepage = "https://github.com/babashka/babashka";
    platforms = [
      "x86_64-linux"
      "x86_64-darwin"
    ];
  };
}
