{ config, pkgs, lib, ... }:

{
	home.packages = with pkgs; [
            bun
	    typescript
	    typescript-language-server
	    gcc
    	    nodejs
	    yarn
	];

	home.sessionVariables = {
	    LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
	};
}
