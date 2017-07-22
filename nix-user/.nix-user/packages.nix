# Custom user packages

with (import <nixpkgs> {}); 
with (import <nixpkgs> {}).xorg; 
with (import ./vim.nix {} );
with (import ./emacs.nix {} );
let
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
  obelisk = import (builtins.fetchTarball https://github.com/rlupton20/alt-nixpkgs/archive/master.tar.gz) {};

  yeganesh = haskellPackages.yeganesh;

  idris = haskellPackages.idris;

  gnupg1compat = pkgs.gnupg1compat.override { gnupg = pkgs.gnupg21; };

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

  cargo = unstable.cargo;
  rustc = unstable.rustc;

  nodejs = unstable.nodejs;
  eslint = unstable.nodePackages.eslint;
  tern = unstable.nodePackages.tern;

  terraform = unstable.terraform;

  powerline = python35Packages.powerline;

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
      custom-emacs;
  };

  haskellTools = {
    inherit
      ghc
      stack;
  };

  rustTools = {
    inherit
      rustc
      cargo
      rustfmt
      rustracer;
  };

  scalaTools = {
    inherit
      scala
      sbt;
  };

  elmTools = {
    inherit
      elm-repl
      elm-reactor
      elm-make
      elm-package;
  };

  scalaTools = {
    inherit
      sbt;

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

  others = {
    inherit
      idris
      rxvt_unicode

      weechat

      firefox
      qutebrowser
      gtypist;
  };

in

(base // haskellTools // rustTools // scalaTools // elmTools // jsTools // opsTools // others)

