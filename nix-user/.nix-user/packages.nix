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
      happy
      hindent
      hasktags
      stylish-haskell
      ghc-mod
      hlint
    ]);

  elm-repl = elmPackages.elm-repl;
  elm-reactor = elmPackages.elm-reactor;
  elm-make = elmPackages.elm-make;
  elm-package = elmPackages.elm-package;

  cargo = unstable.cargo;
  rustc = unstable.rustc;

  powerline = unstable.python35Packages.powerline;

  base = {
    inherit
      stdenv
      gcc
      ack
      global
    
      ranger
      zathura
      htop
      yeganesh
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

  elmTools = {
    inherit
      elm-repl
      elm-reactor
      elm-make
      elm-package;
  };

  others = {
    inherit
      idris

      weechat

      firefox
      vimb
      gtypist;
  };

in

(base // haskellTools // rustTools // elmTools // others)

