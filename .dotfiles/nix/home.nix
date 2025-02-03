{ pkgs }:
with pkgs;
{

  # * Home Manager
  # use home manager as nix-darwin module, so that user profiles are built
  # together with the system when running darwin-rebuild
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = {
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};

      home.stateVersion = "25.05";

      # extra directories to add to path
      home.sessionPath = [
        "${user}/bin"
      ];
      programs.neovim = {
              enable = true;
              extraLuaPackages = ps: [ ps.magick ];
              extraPackages = [ pkgs.imagemagick ];
          };

    };
  };
}
