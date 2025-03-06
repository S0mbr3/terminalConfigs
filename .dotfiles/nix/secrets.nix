{ config ? {}, lib, pkg-config ? {}, ... }:
let
    secrets = builtins.importAge [ ./secret-key ] ./secret.nix.age {};
in
{
  # Declare the option first
  options = {
    rage-username = lib.mkOption {
      type = lib.types.str;
      description = "Username encrypted with age";
      default = "tmp";
    };
  };
  options = {
    rage-hostName = lib.mkOption {
      type = lib.types.str;
      description = "hostName encrypted with age";
      default = "tmpHostName";
    };
  };
  options = {
    rage-syncthingUser = lib.mkOption {
      type = lib.types.str;
      description = "Syncthing user encrypted with age";
      default = "tmpSyncthingUser";
    };
  };
  config = {
    rage-username = secrets.rage-username;
    rage-hostName = secrets.rage-hostName;
    rage-syncthingUser = secrets.rage-syncthingUser;
    };
}
