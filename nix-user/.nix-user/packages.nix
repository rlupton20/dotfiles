# Custom user packages

with (import <nixpkgs> {}); 
with (import <nixpkgs> {}).haskellPackages; 
with (import <nixpkgs> {}).xorg; 
with (import ./vim.nix {} );
with (import ./emacs.nix {} );
let
  gnupg1compat = pkgs.gnupg1compat.override { gnupg = pkgs.gnupg21; };
in
{
  inherit
    ranger
    zathura
    htop
    yeganesh
    custom-vim
    custom-emacs

    weechat
    
    gnupg1compat

    firefox
    
    xmodmap
    gtypist; }
