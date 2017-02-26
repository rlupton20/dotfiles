# Custom user packages

with (import <nixpkgs> {}); 
with (import <nixpkgs> {}).xorg; 
with (import ./vim.nix {} );
with (import ./emacs.nix {} );
let
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};

  yeganesh = haskellPackages.yeganesh;

  stgit = gitAndTools.stgit;

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
#  elm-oracle = elmPackages.elm-oracle;
#  elm-format = elmPackages.elm-format;
#  elm-test = elmPackages.elm-test;

  powerline = unstable.python35Packages.powerline;
in
{
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

    custom-vim
    custom-emacs

    ghc
    stack

    idris

    rustc
    cargo
    rustfmt
    rustracer

    elm-repl
    elm-reactor
    elm-make
    elm-package
#    elm-oracle
#    elm-format
#    elm-test
    
    weechat
    
    gnupg1compat

    firefox
    vimb
    
    xmodmap
    gtypist; }
