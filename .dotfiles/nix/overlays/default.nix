/* {inputs, ...}: {
# This one brings our custom packages from the 'pkgs' directory
additions = final: _prev: import ../pkgs {pkgs = final;};

# https://nixos.wiki/wiki/Overlays
modifications = final: prev: {
emacsLeji = import ./emacs.nix {inherit prev;};
};
} */
{inputs}:

[
  (import ./emacs.nix)
]

/* {inputs}: {
  
  modifications = final: prev: {
    emacsLeji = import ./emacs.nix {inherit prev;};
  };
} */
/* {inputs}:

[
  (final: prev: {
    emacsLeji = import ./emacs.nix {inherit prev;};
  })
] */
