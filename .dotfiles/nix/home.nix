#{ self, pkgs, inpts, config,... }:
{ config, pkgs, lib, ... }:

  # * Home Manager
  # use home manager as nix-darwin module, so that user profiles are built
  # together with the system when running darwin-rebuild
rec {
  home.username = config.rage-username;
  home.homeDirectory = builtins.trace "configdir: ${config.home.username}" "/Users/${config.home.username}";
      home.enableNixpkgsReleaseCheck = false;
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

  home.packages = with pkgs; [ ueberzugpp ];

  programs.ranger = {
    enable = true;
    extraConfig = ''
      set preview_images true
      set preview_images_method ueberzug
      '';
  };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };


  home.file = {
    ".emacs.d" = {
      source = ../emacs/.emacs.d;
    };
    ".config/ueberzugpp" = {
      source = ../ueberzugpp/.config/ueberzugpp;
    };
  };

   home.activation.decryptPassword = lib.hm.dag.entryAfter ["writeBoundary"] ''
    ${pkgs.age}/bin/age --decrypt -i ~/.config/sops/age/keys.txt ${config.home.homeDirectory}/terminalConfigs/.dotfiles/nix/.passwordFile.age > ${config.home.homeDirectory}/.passwordFile
  '';

    services = {
    syncthing = {
      enable = true;
      #passwordFile = ./password;
      passwordFile = "${config.home.homeDirectory}/.passwordFile";
      overrideDevices = true;     # overrides any devices added or deleted through the WebUI
      overrideFolders = true;     # overrides any folders added or deleted through the WebUI
      settings = {
	options = {
	  urAccepted = -1;
	};
	devices = {
	  nebj = {
	    name = "nebj";
	    id = "JXZ3KCU-NCNNCU3-ZQK2UZ6-VHDYHVP-I2FUBUN-E4DTAAB-VNVHKVO-LYAXSAB";
	    autoAcceptFolders = true;
	  };
	  android = {
	    name = "android";
	    id = "A5KISFU-IBH7OH2-7OLMT7V-3YCDSRP-VCLXWLR-ECBRQNF-MP5PNSO-ZJSD3AA";
	    autoAcceptFolders = true;
	  };
	};
	# folders = {
	#   "org" = {
	#     path = "~/org";
	#     devices = ["nebj" "android"];
	#   };
	# };
	gui = {
	  user = config.rage-syncthingUser;
	};
      };
      #extraOptions = ["cli config user set patata "];
    };
  };
  launchd.agents.removePassword = {
    enable = false;
    config = {
      ProgramArguments = [
	"${pkgs.bash}/bin/bash"
	"-c"
	''
	  while ! pgrep syncthing >/dev/null; do sleep 1; done
	  sleep 5
          ${pkgs.coreutils}/bin/shred -u ${config.home.homeDirectory}/.passwordFile
	''
      ];
      Label = "com.yourdomain.removePassword";
      StartInterval = 10;  # Runs every 10 seconds, modify as needed
      RunAtLoad = true;    # Runs at startup
      KeepAlive = false;   # Don't restart once it runs
    StandardOutPath = "/tmp/removePassword.out.log";
    StandardErrorPath = "/tmp/removePassword.err.log";
    };
  };

    }
