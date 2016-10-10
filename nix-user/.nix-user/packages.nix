# Custom user packages

with (import <nixpkgs> {}); with (import <nixpkgs> {}).haskellPackages; with (import <nixpkgs> {}).xorg; with (import ./vim.nix {} );
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

    weechat
    
    gnupg21
    gnupg1compat
    
    qemu
    
    stack

    virtinst
    virt-viewer

    xmodmap
    gtypist; }
