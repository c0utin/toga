{ config, pkgs, lib, ... }:

{
	home.packages = with pkgs; [
            bun
	    typescript
	    typescript-language-server
	];
}
