# Custom user packages

let

  # First we configure our required packages
  rust-overlay = import ((builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz)+"/rust-overlay.nix");

  pkgs = import <nixpkgs> { 
    overlays = [
      rust-overlay
    ]; 
  }; 

in with pkgs; let

  # Additional package sets
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  obelisk = import (builtins.fetchTarball https://github.com/rlupton20/alt-nixpkgs/archive/master.tar.gz) {};

  # Bring our custom vims into scope
  vims = import ./vim.nix { inherit pkgs; };
  custom-vim = vims.custom-vim;
  custom-neovim = vims.custom-neovim;

  # Bring our custom emacs into scope
  emacsen = import ./emacs.nix { inherit pkgs; };
  custom-emacs = emacsen.custom-emacs;

  # We need to specify some other packages more precisely, which we do here
  yeganesh = haskellPackages.yeganesh;
  xmobar = haskellPackages.xmobar;

  xinput = xorg.xinput;
  xmodmap = xorg.xmodmap;

  qutebrowser = pkgs.qutebrowser.override {
    withWebEngineDefault = true;
  };

  idris = haskellPackages.idris;

  gpg = gnupg1;

  ghc = haskellPackages.ghcWithHoogle(packages: with packages; [
      hindent
      hasktags
      stylish-haskell
      hlint
    ]);

  elm-repl = unstable.elmPackages.elm-repl;
  elm-reactor = unstable.elmPackages.elm-reactor;
  elm-make = unstable.elmPackages.elm-make;
  elm-package = unstable.elmPackages.elm-package;
  elm-format = unstable.elmPackages.elm-format;

  rust = latest.rustChannels.nightly.rust.override { 
    extensions = [ 
      "rust-src" 
    ]; 
  };

  nodejs = unstable.nodejs;
  eslint = unstable.nodePackages.eslint;
  tern = unstable.nodePackages.tern;

  terraform = unstable.terraform;

  # LaTeX installation
  texlive-collection = texlive.combine {
    inherit
      (texlive)
      scheme-basic;
  };

  powerline = python35Packages.powerline;

  csv = obelisk.miniTools.csv;

  # Package groups (makes it easier to thin down an install)
  base = {
    inherit
      stdenv
      gcc
      ack
      ag
      global
      fzf
    
      ranger
      zathura
      htop
      yeganesh
      rofi
      tmux
      powerline
      xmodmap
      gpg

      custom-vim
      custom-neovim
      custom-emacs;
  };

  haskellTools = {
    inherit
      ghc
      stack;
  };

  rustTools = {
    inherit
      rust
      rustfmt
      rustracer;
  };

  scalaTools = {
    inherit
      scala
      sbt;
  };

  clojureTools = {
    inherit
      leiningen;
  };

  elmTools = {
    inherit
      elm-repl
      elm-reactor
      elm-make
      elm-package
      elm-format;
  };

  jsTools = {
    inherit
      nodejs
      tern
      eslint;
  };

  opsTools = {
    inherit
      virtinst
      virt-viewer
      terraform;
  };

  xmonadSupport = {
    inherit
      rxvt_unicode
      xmobar
      maim
      slop
      xclip
      xinput
      i3lock;
  };

  latexTools = {
    inherit
      texlive-collection;
  };

  myTools = {
    inherit
      csv;
  };

  others = {
    inherit
      idris

      weechat

      firefox
      qutebrowser
      gtypist;
  };

in
# Package groups we want installed
(base // haskellTools // rustTools // scalaTools // clojureTools // elmTools // jsTools // opsTools // xmonadSupport // latexTools // myTools // others)

