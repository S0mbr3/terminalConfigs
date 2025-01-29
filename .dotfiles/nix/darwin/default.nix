{ self, config, pkgs, ...}:
let
  user = builtins.readfile ./username;
  homeDir = "Users/${user}";
  configDir = "${homeDir}/.config";
  cacheDir = "${homeDir}/.cache";

  # Import the package list
  packageList = import ./packages.nix { pkgs = pkgs; };
in
  {
  # https://nix-community.github.io/home-manager/index.html#sec-install-nix-darwin-module
  imports = [
    <home-manager/nix-darwin>
  ];

  # * Nix
  # auto upgrade nix package and the daemon service
  service.nix-daemon.enable = true;

  nix = {
    package = pkgs.nixUnstable;
    settings.trusted-users = [ "root" "@admin" ];

    nix.settings.experimental-features = "nix-command flakes";

    #automatically gargave collect to reduce nix store size
    gc = {
      user = "root";
      automatic = true;
      interval = { Weekday = 0; Hour = 2; Minute = 0; };
      options = "--delete-older-than 15d";
    };

  };

  # * Environment
  # installing both here with home manager
  # TODO split system wise packages with home wise packages
  environment.systempackages = packageList;

  # * Fonts
  # don't fully manage fontdir (will remove any manually installed fonts)
  # (default is false)
  fonts.fontDir.enable = false;

  # fonts to install
  fonts.fonts = [
    # https://github.com/NixOS/nixpkgs/blob/master/pkgs/data/fonts/nerdfonts/shas.nix
    (pkgs.nerdfonts.override {
      fonts = ["CascadiaCode" "FiraCode" "FiraMono"];
    })
    pkgs.cascadia-code
    pkgs.office-code-pro
  ];

  homebrew = {
    enable = true;
    taps = [
      "homebrew/core"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "koekeishiya/formulae"
    ];
    brews = [
      "progress"
    ];
    casks = [

    ];
  };

  programs.zsh.enable = true;

  # * System Settings
  system = {

    # Set Git commit hash for darwin-version.
    configurationRevision = self.rev or self.dirtyRev or null;
    defaults = {

      # ** Appearance
      # dark mode
      NSGlobalDomain.AppleInterfaceStyle = "Dark";

      # ** Menu Bar
      NSGlobalDomain._HIHideMenuBar = true;

      # ** Dock, Mission Control
      dock = {
        autohide = true;
        # make smaller (default 64)
        titlesize = 48;
      };

      # ** Keyboard
      NSGlobalDomain.InitialKeyRepeat = 20;
      NSGlobalDomain.KeyRepeat = 1;

      # ** Mouse
      # enable tap to click
      trackpad.Clicking = true;
      NSGlobalDomain."come.apple.mouse.tapBehavior" = 1;

      # disable natural scroll direction
      NSGlobalDomain."com.apple.swipescrolldirection" = false;

      # ** Finder
      # don't show desktop icons
      finder.CreateDesktop = false;

      NSGlobalDomain.AppleShowAllExtensions = true;
      finder.AppleShowAllextensions = true;

      # default to list view
      finder._FXPreferredViewStyle = "Nlsv";

      # full path in window title
      finder._FxShowPosixPathInTitle = true;
    };
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 6;

  # The platform the configuration will be used on.
  # nixpkgs.hostPlatform = "aarch64-darwin";

  # * Users
  users.users.${user} = {
    name = user;
    home = homeDir;
  };

  # * Home Manager
  # use home manager as nix-darwin module, so that user profiles are built
  # together with the system when running darwin-rebuild
  home-manager = {
    useGlobalPkgs = true;
    user.${user} = {
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};

      home.stateVersion = "25.05";

      # extra directories to add to path
      home.sessionPath = [
        "${user}/bin"
      ];

    };
  };

}

