{ config, pkgs, lib, ... }:

let
  zigFromTarball = pkgs.stdenv.mkDerivation {
    pname = "zig";
    version = "0.15.1";

    src = pkgs.fetchurl {
      url = "https://ziglang.org/download/0.15.1/zig-x86_64-linux-0.15.1.tar.xz";
      sha256 = "sha256-xhxdpu3uoUylHs1eRSDG9Bie9SUDg9sz0BhIKTv6/gU=";
    };

    dontConfigure = true;
    dontBuild = true;
    dontStrip = true;

    installPhase = ''
      mkdir -p $out
      cp -r ./* $out/
      mkdir -p $out/bin
      ln -s $out/zig $out/bin/zig
    '';
  };
in
{
  home.packages = [
    zigFromTarball
    pkgs.zls
  ];
}