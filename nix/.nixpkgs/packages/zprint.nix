{ stdenv ? (import <nixpkgs> {}).stdenv
, fetchurl ? (import <nixpkgs> {}).fetchurl }:

let appName = "zprint";
    version = "1.0.2";
    binary = {
      "x86_64-linux" = "zprintl";
      "x86_64-darwin" = "zprintm";
    }."${stdenv.system}" or (throw "Unsupported platform");
    sha256 = {
      "x86_64-linux" = "064yavqszw84212y31zpawfia6158v44rq3rh3v8y3qzjaqfp3my";
      "x86_64-darwin" = "0k8fa2g4xmc6zdxn335247vv8h2mhwxwxi2i1yj0xnyirdh5p6y3";
    }."${stdenv.system}" or (throw "Unsupported platform");

in stdenv.mkDerivation {
  pname = appName;
  version = version;

  phases = ["installPhase"];

  src = fetchurl {
    url = "https://github.com/kkinnear/${appName}/releases/download/${version}/${binary}-${version}";
    sha256 = "${sha256}";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/${appName}
    chmod +x $out/bin/${appName}
  '';

  meta = {
    description = "Library to reformat Clojure and Clojurescript source code and s-expressions";
    license = stdenv.lib.licenses.mit;
    homepage = "https://github.com/kkinnear/zprint";
    platforms = [
      "x86_64-linux"
      "x86_64-darwin"
    ];
  };
}
