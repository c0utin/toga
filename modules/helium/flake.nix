{
  description = "Helium AppImage wrapper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      packages.${system}.default = pkgs.appimageTools.wrapType2 {
        pname = "helium";
        version = "0.5.6.1"; 
        src = pkgs.fetchurl {
          url = "https://github.com/imputnet/helium-linux/releases/download/0.6.3.1/helium-0.6.3.1-x86_64.AppImage";
          sha256 = "0ifmvnknppvik79yajknqkc7v3qwk9c3gplarlp7dclxncn6kcip";
        };
      };
    };
}

