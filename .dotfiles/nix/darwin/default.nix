{ self, pkgs, ...}:
let
  #user = builtins.readfile ./username;
  #user = "Nebj";
  #user = builtins.getEnv "USER";
  user = "nebj";
  homeDir = "/Users/${user}";
  configDir = "${homeDir}/.config";
  cacheDir = "${homeDir}/.cache";

  # Import the package list
  packageList = import ./packages.nix  { pkgs = pkgs; };
in
  {
  imports = [
    ./pam-reattach.nix
  ];

  # * Nix
  # auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  nix = {
    #package = pkgs.nixUnstable;
    package = pkgs.nixVersions.latest;
    settings.trusted-users = [ "root" "@admin" ];

    settings.experimental-features = "nix-command flakes";

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
  environment.systemPackages = packageList;

  # * Fonts
  # don't fully manage fontdir (will remove any manually installed fonts)
  # (default is false)
  #fonts.fontDir.enable = false;

  # fonts to install
  fonts.packages = [
    pkgs.cascadia-code
    pkgs.office-code-pro
    pkgs.fira-code
    pkgs.fira-mono
    pkgs.nerd-fonts.hack
  ];

  homebrew = {
    enable = true;
    taps = [
      "koekeishiya/formulae"
      #To install JankyBoders
      "FelixKratz/formulae"
    ];
    brews = [
      "borders"
      "sketchybar"
      "progress"
      "pam-reattach"
      "reattach-to-user-namespace"
    ];
    casks = [
      "iterm2"
      "nikitabobko/tap/aerospace"
    ];
  };

  programs.zsh.enable = true;

  # To use TouchId for sudo operations
  security.pam.enableSudoTouchIdAuth = true;
  # Make TouchId for sudo operations work with tmux (see ./pam-reattach.nix)
  security.pam.enableSudoTouchIdReattach = true;

  # * System Settings
  system = {

    # Set Git commit hash for darwin-version.
    configurationRevision = self.rev or self.dirtyRev or null;
    defaults = {

      # ** Appearance
      # dark mode
      NSGlobalDomain.AppleInterfaceStyle = "Dark";

      # ** Menu Bar
      NSGlobalDomain._HIHideMenuBar = false;

      # ** Dock, Mission Control
      dock = {
        autohide = true;
        # make smaller (default 64)
        tilesize = 48;
      };

      # ** Keyboard
      NSGlobalDomain.InitialKeyRepeat = 20;
      NSGlobalDomain.KeyRepeat = 1;

      # ** Mouse
      # enable tap to click
      trackpad.Clicking = true;
      NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;

      # disable natural scroll direction
      NSGlobalDomain."com.apple.swipescrolldirection" = false;

      # ** Finder
      # don't show desktop icons
      finder.CreateDesktop = false;

      NSGlobalDomain.AppleShowAllExtensions = true;
      finder.AppleShowAllExtensions = true;

      # default to list view
      finder.FXPreferredViewStyle = "Nlsv";

      # full path in window title
      finder._FXShowPosixPathInTitle = true;
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

}

