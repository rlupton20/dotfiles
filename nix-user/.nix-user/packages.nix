# Custom user packages

with (import <nixpkgs> {}); 
with (import <nixpkgs> {}).haskellPackages; 
with (import <nixpkgs> {}).xorg; 
with (import ./vim.nix {} );
with (import ./emacs.nix {} );
let
  gnupg1compat = pkgs.gnupg1compat.override { gnupg = pkgs.gnupg21; };
  ghc = haskellPackages.ghcWithHoogle(packages: with packages; [
      happy
      hindent
      hasktags
      stylish-haskell
      ghc-mod
      hlint
    ]);
in
{
  inherit
    stdenv
    
    ranger
    zathura
    htop
    yeganesh
    custom-vim
    custom-emacs

    ghc

    rustc
    cargo
    rustfmt
    racer
    
    weechat
    
    gnupg1compat

    firefox
    
    xmodmap
    gtypist; }
