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
  config = {
    rage-username = secrets.rage-username;
    };
}
