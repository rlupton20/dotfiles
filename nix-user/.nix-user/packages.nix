# Custom user packages

with (import <nixpkgs> {}); with (import <nixpkgs> {}).haskellPackages; with (import ./vim.nix {} );
let
  gnupg1compat = pkgs.gnupg1compat.override { gnupg = pkgs.gnupg21; };
in
{
  inherit
    emacs
    ranger
    zathura
    htop
    yeganesh
    custom-vim
    
    gnupg21
    gnupg1compat
    
    qemu
    
    stack

    gtypist; }
