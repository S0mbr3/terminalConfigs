{inputs, ...}:
{
  imports = [
    #inputs.sops-nix.darwinModules.sops
    inputs.sops-nix.homeManagerModules.sops
  ];
  #sops.age.keyFile = "${configDir}/sops/age/keys.txt";
  sops = {
    age.keyFile = "/etc/sops/age/keys.txt";
    defaultSopsFile = ./secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    secrets.example-key = {};
    secrets."myservice/my_subdir/my_secret" = {};
  };
}
