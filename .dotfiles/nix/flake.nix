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
  };

  outputs = inputs@{ self, home-manager, nix-darwin, nixpkgs, mac-app-util }:
    let
      pkg-config = {
        allowUnfree = true;
        allowBroken = true;
        allowInsecure = false;
      };
      darwin-pkgs = import nixpkgs {
        # M1
        system = "aarch64-darwin";
        config = pkg-config;
      };
    in
      {
      # Build darwin flake using:
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
