{ pkgs, ... }:

  # * Home Manager
  # use home manager as nix-darwin module, so that user profiles are built
  # together with the system when running darwin-rebuild
{
  home.username = "nebj";
  home.homeDirectory = "/Users/nebj";
      home.enableNixpkgsReleaseCheck = false;
      # home.packages = pkgs.callPackage ./packages.nix {};
      # home.packages = with pkgs;[
      #   home-manager
      #   ];

      home.stateVersion = "25.05";

      # extra directories to add to path
      home.sessionPath = [
        "$HOME/bin"
      ];
      programs.neovim = {
              enable = true;
              extraLuaPackages = ps: [ ps.magick ];
              extraPackages = [ pkgs.imagemagick ];
          };
      programs.home-manager.enable = true;
      programs.zsh.enable = false;

      home.file.".emacs.d" = {
        source = ../emacs/.emacs.d;
      };
    }

