{
  description = "Athas AppImage wrapper";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      packages.${system}.default = pkgs.appimageTools.wrapType2 {
        pname = "athas";
        version = "0.2.4"; 
        src = pkgs.fetchurl {
          url = "https://github.com/athasdev/athas/releases/download/v0.2.4/Athas_0.2.4_amd64_linux.AppImage";
          sha256 = "1q6yvqw3r9rr8d8bjnmnaw0mz5jd1jcaq8s3lza2pn3g59hfg4i5";
        };
      };
    };
}

