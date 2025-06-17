{ config, pkgs, inputs, system, ... }:

{

  imports = [
    ./languages/go.nix
    ./languages/js.nix  
    ./languages/femboy.nix
  ];

  home.username = "toga";
  home.homeDirectory = "/home/toga";

  home.stateVersion = "24.11";


  # :(
  nixpkgs.config.allowUnfree = true;
 
  home.packages = with pkgs; [
    nushell
    emacs
    zed-editor
    obs-studio
    nushell    
    code-cursor 
    audacity
    zapzap
    vesktop
    calibre
    mtpfs 
    gnumake
    taskwarrior3
    taskwarrior-tui
    postgresql
    dbeaver-bin
    google-cloud-sdk
    docker-compose
    zsh
    oh-my-zsh
    helix
    google-chrome
    xorg.xmodmap
    blueman
    gex


    # flake
    inputs.zen-browser.packages.${pkgs.system}.default
  ];


 # /.config 
  home.file.".zshrc".source = "${inputs.self}/modules/zsh/zshrc";
  programs.zsh = {
    enable = true;
    initExtra = ''
      source ${inputs.self}/modules/zsh/zshrc
    '';
  };

	# i3
	xdg.configFile."i3/config".source = "${inputs.self}/modules/i3/config";

	# emacs
	xdg.configFile."emacs/init.el".source = "${inputs.self}/modules/emacs/init.el";

	# xmodmap (corne)
	home.file.".Xmodmap".source = "${inputs.self}/modules/xmodmap/Xmodmap";

	# helix
	xdg.configFile."helix/config.toml".source = "${inputs.self}/modules/helix/config.toml";
	xdg.configFile."helix/themes".source = "${inputs.self}/modules/helix/themes";


	
  home.sessionVariables = {
     EDITOR = "emacs";
     SHELL  = "${pkgs.zsh}/bin/zsh";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  	programs.git = {
		enable = true;
		package = pkgs.gitFull;
		userName = "c0utin";
		userEmail = "rafaelcouto111@gmail.com";
		extraConfig.init.defaultBranch = "main";
	};

}
