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


    # flake
    inputs.zen-browser.packages.${pkgs.system}.default
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

  };


  # /.config 

	# i3
	xdg.configFile."i3/config".source = "${inputs.self}/modules/i3/config";

	# emacs
	xdg.configFile."emacs/init.el".source = "${inputs.self}/modules/emacs/init.el";


  home.sessionVariables = {
     EDITOR = "emacs";
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
