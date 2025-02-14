# my custom Emacs
# use emacs-plus patches on osx
# (eventually) use lucid on linux

# relevant links:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/generic.nix
# https://github.com/nix-community/emacs-overlay/blob/master/overlays/emacs.nix
# https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30
# {prev}:
final: prev: rec {
  # configuration shared for all systems
  emacsGitLejiGeneric = (prev.emacs-git.override {
    withSQLite3 = true;
    withWebP = true;
    withImageMagick = true;
    withTreeSitter = true;
    withMailutils = true;
    withNativeCompilation = true;
  }).overrideAttrs (oldAttrs: {
      env = (oldAttrs.env or {}) // {
        NIX_CFLAGS_COMPILE = (oldAttrs.env.NIX_CFLAGS_COMPILE or "") + " -O3 -march=native";
      };
    });
  emacsLeji =
    if prev.stdenv.isDarwin
    then
      emacsGitLejiGeneric.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            /* # Don't raise another frame when closing a frame
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
              sha256 = "1gfabyhwad923aq69mdzzvbdlgplpk16bbj3y54p780ai7f7fmzv";
            }) */
            # Fix OS window role so that yabai can pick up Emacs
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            })
            # Add setting to enable rounded window with no decoration (still
            # have to alter default-frame-alist)
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/round-undecorated-frame.patch";
              sha256 = "/SX8rF4GMA7bobfQ4/F9BTSEigeOd9jgN0jvQ1M0MSs=";
            })
            # Make Emacs aware of OS-level light/dark mode
            # https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
            (prev.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
              sha256 = "3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
            })
            # # Fix alpha-background not working on macOS NS build
            # (prev.fetchpatch {
            #   url = "https://raw.githubusercontent.com/jimeh/build-emacs-for-macos/master/patches/emacs-29/ns-alpha-background.patch";
            #   sha256 = "1i39zjyjqw09j08if84pdlpi38x9blzrlqgihbz6d87glc6a73yy";
            # })
            ./ns_alpha_background.patch
          ];
        withNS = true;
        # withPgtk = true;
        /* configureFlags = [
          "--with-ns"
          "--with-mailutils"
          "--with-imagemgick"
          "--with-wide-int"
          "--with-tree-sitter"
          "--with-native-compilation"
          "--with-xwidgets"
          "--width-dbus"
          "--program-transform-name='s/^ctags$/emctags/' \ # avoid ctags namespace conflict"

        ]; */
      })
    else
      # TODO nix's lucid reports the wrong mm-size (breaks textsize package):
      # (frame-monitor-attribute 'mm-size (selected-frame))
      (emacsGitLejiGeneric.override {
        withX = true;
        # lucid
        # withGTK2 = false;
        withGTK3 = true;
        withXinput2 = true;
      }).overrideAttrs(_: {
        # for full control/testing (e.g. can't do lucid without cairo using
        # builtin withs)
        configureFlags = [
          # for a (more) reproducible build
          "--disable-build-details"
          "--with-modules"
          "--with-x-toolkit=gtk3"
          "--with-xft"
          "--with-cairo"
          "--with-xaw3d"
          "--with-native-compilation"
          "--with-imagemagick"
          "--with-xinput2"
        ];
      });
  emacsLejiWithPackages =
    ((prev.emacsPackagesFor emacsLeji).emacsWithPackages (epkgs: [
      # necessary to install through nix to get libenchant integration working
      epkgs.jinx
      # not needed on linux but needed on mac
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
      # epkgs.all-the-icons
    ]));

  # for Wayland builds
  emacsLejiWayland =
    (emacsGitLejiGeneric.override {
      withX = false;
      withPgtk = true;
    });
  emacsPgtkWithPackages =
    ((prev.emacsPackagesFor emacsLejiWayland).emacsWithPackages (epkgs: [
      # necessary to install through nix to get libenchant integration working
      epkgs.jinx
      # not needed but prevents need to compile on first run
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
    ]));
}
