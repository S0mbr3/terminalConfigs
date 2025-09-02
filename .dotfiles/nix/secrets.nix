{
  lib,
  ...
}:
{
  # Declare the option first
  options.rage = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "Username encrypted with age";
      default = "tmp";
    };
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "hostName encrypted with age";
      default = "tmpHostName";
    };
    syncthingUser = lib.mkOption {
      type = lib.types.str;
      description = "Syncthing user encrypted with age";
      default = "tmpSyncthingUser";
    };
  };
  config =
    let
      keyFile = ./secret-key;
      secrets = builtins.importAge [ keyFile ] ./secret.nix.age { cache = false; };
    in
    {
      rage = {
        username = secrets.rage-username;
        hostName = secrets.rage-hostName;
        syncthingUser = secrets.rage-syncthingUser;
      };
    };
}
