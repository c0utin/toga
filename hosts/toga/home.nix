{ config, pkgs, inputs, system, ... }:

{
  home.username = "toga";
  home.homeDirectory = "/home/toga";

  home.stateVersion = "24.11";
 
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    nushell
    emacs
    zed-editor
    obs-studio
    nushell    

    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })


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

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };


  # /.config 

	# i3
	xdg.configFile."i3/config".source = "${inputs.self}/modules/i3/config";

  home.sessionVariables = {
    # EDITOR = "emacs";
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
