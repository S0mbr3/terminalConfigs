{
  description = "Nix configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util.url = "github:hraban/mac-app-util";

    # https://nixos.wiki/wiki/Emacs
    # https://nixos.wiki/wiki/Overlays#In_a_Nix_flake
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, home-manager, nix-darwin, nixpkgs, mac-app-util, emacs-overlay }:
    let
      pkg-config = {
        allowUnfree = true;
        allowBroken = true;
        allowInsecure = false;
      };
      common-overlays =
        [
          # default emacs-overlay (overriden by ./overlays/emacs.nix)
          (import emacs-overlay)
        ] ++ import ./overlays {inherit inputs;};
      darwin-pkgs = import nixpkgs {
        # M1
        system = "aarch64-darwin";
        config = pkg-config;
        # overlays = [(import ./overlays/emacs.nix)];
        # overlays = import ./overlays {inherit inputs;};
        overlays = common-overlays;
        # overlays = [ (import ./overlays/emacs.nix) ];
      };
    in
      {
      # overlays = import ./overlays {inherit inputs;};
      # overlays = [(import ./overlays)];
      # # Build darwin flake using:
      # $ darwin-rebuild switch .
      darwinConfigurations.default = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [
          mac-app-util.darwinModules.default
          home-manager.darwinModules.home-manager
          (
            {pkgs, config, inputs, ...}:
            {
              # To enable it for all users:
              home-manager.sharedModules = [
                mac-app-util.homeManagerModules.default
              ];
            }
          )
          ./darwin
          ./home.nix
        ];
        #specialArgs = { inherit inputs; };
      };
      darwinConfigurations."MacBook-Air-de-Aude" = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [
          mac-app-util.darwinModules.default
          home-manager.darwinModules.home-manager
          ./darwin
        ];
        specialArgs = { inherit self inputs; };
      };
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."simple" = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [ ./darwin ];
      };
    };
}
