# Custom user packages

with (import <nixpkgs> { overlays = [
  (import ((builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz)+"/rust-overlay.nix"))
  ]; 
}); 
with (import <nixpkgs> {}).xorg; 
with (import ./vim.nix {} );
with (import ./emacs.nix {} );
let
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  obelisk = import (builtins.fetchTarball https://github.com/rlupton20/alt-nixpkgs/archive/master.tar.gz) {};

  yeganesh = haskellPackages.yeganesh;
  xmobar = haskellPackages.xmobar;

  qutebrowser = pkgs.qutebrowser.override {
    withWebEngineDefault = true;
  };

  idris = haskellPackages.idris;

  gnupg1compat = pkgs.gnupg1;

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

  rust = latest.rustChannels.nightly.rust;

  nodejs = unstable.nodejs;
  eslint = unstable.nodePackages.eslint;
  tern = unstable.nodePackages.tern;

  terraform = unstable.terraform;

  texlive-collection = texlive.combine {
    inherit
      (texlive)
      scheme-basic;
  };

  powerline = python35Packages.powerline;

  csv = obelisk.miniTools.csv;

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
      gnupg1compat

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

(base // haskellTools // rustTools // scalaTools // clojureTools // elmTools // jsTools // opsTools // xmonadSupport // latexTools // myTools // others)

