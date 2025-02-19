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
    sops-nix.url = "github:Mic92/sops-nix";

    # https://nixos.wiki/wiki/Emacs
    # https://nixos.wiki/wiki/Overlays#In_a_Nix_flake
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, home-manager, nix-darwin, nixpkgs, mac-app-util, emacs-overlay, sops-nix}:
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
        overlays = common-overlays;
      };
    in
      {
      # $ darwin-rebuild switch .
      darwinConfigurations.default = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [
          mac-app-util.darwinModules.default
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.nebj = import ./home.nix;

          }
          ./darwin
        ];
        specialArgs = { inherit self inputs; };
      };
      darwinConfigurations."MacBook-Air-de-Aude" = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [
          mac-app-util.darwinModules.default
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.nebj = import ./home.nix;
          }
          ./darwin
	  # sops-nix.nixosModules.sops
        ];
        specialArgs = { inherit self inputs; };
      };
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."simple" = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        system = "aarch64-darwin";
        modules = [
          ./darwin
          { nixpkgs.overlays = import ./overlays; }
        ];
      };
    };
}
