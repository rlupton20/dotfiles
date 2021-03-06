################################################################################
## NIX MANIFEST
## Richard Lupton 2017
##
## This nix expression exposes all the packages which I want installed on the
## system. Packages can be installed with
##   $ nix-env -f file_name.nix -ir
##
## There are 4 main sections to this file
##   1) The first section configures pkgs to be nixpkgs with any required
##      customizations or overlays.
##   2) The second describes any custom packages/customizations to existing
##      packages.
##   3) Packages are then grouped into package groups. This is so the
##      final set can be described as a merge of these groups, which allows
##      for easier selection of tools on a system where not everything is
##      required.
##   4) The fourth section is a merge of attribute sets (package groups).
##      All these packages will be installed when the above shell command
##      is run.
################################################################################

let
  ##############################################################################
  ## PACKAGE SET CONFIGURATION
  ##############################################################################
  _fetchFromGitHub = (import <nixpkgs> {}).fetchFromGitHub;
  _licenses = (import <nixpkgs> {}).stdenv.lib.licenses;

  nixpkgs-mozilla = _fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "6179dd876578ca2931f864627598ede16ba6cdef";
    sha256 = "1lim10a674621zayz90nhwiynlakxry8fyz1x209g9bdm38zy3av";
  };

  obelisk-nixpkgs = _fetchFromGitHub {
    owner = "rlupton20";
    repo = "alt-nixpkgs";
    rev = "2323318c7ad0c72934e9e0e01be82073d7c1f89e";
    sha256 = "0g0z3v4l6xs8r0apjzhs9g5fq87hjl59dpwbw9msh9psk6kjp321";
  };

  nxe-repository = _fetchFromGitHub {
    owner = "rlupton20";
    repo = "nxe";
    rev = "a80f4a86775ef3bec91c0c52abbf848c1f88b423";
    sha256 = "0x3z2lyqrncs90jfyrgl9qbc017c75qky79nbfj637cb2l95dxrh";
  };

  rust-overlay = import "${nixpkgs-mozilla}/rust-overlay.nix"; 
  obelisk-overlay = import "${obelisk-nixpkgs}/overlay.nix";
  nxe-overlay = import "${nxe-repository}/overlay.nix";

  # Define pkgs as <nixpkgs> with some overlays
  # Also blacklist unfree licenses
  pkgs = import <nixpkgs> { 
    overlays = [
      rust-overlay
      obelisk-overlay
      nxe-overlay
    ]; 
    config = {
      blacklistedLicenses = with _licenses; [
        unfreeRedistributableFirmware
      ];
    };
  }; 

in with pkgs; let
  ##############################################################################
  ## PACKAGE CUSTOMIZATIONS
  ##############################################################################

  # Additional package sets
  unstable = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};

  # Bring our custom vims into scope
  vims = import ./vim.nix { inherit pkgs; };
  custom-vim = vims.custom-vim;
  custom-neovim = vims.custom-neovim;

  # Bring our custom emacs into scope
  custom-emacs = (import ./emacs.nix { inherit pkgs; }).custom-emacs;

  custom-tmux = callPackage ./tmux.nix { powerline = python35Packages.powerline; };

  # We need to specify some other packages more precisely, which we do here
  qutebrowser = pkgs.qutebrowser.override {
    withWebEngineDefault = true;
  };

  ghc = haskellPackages.ghcWithHoogle(packages: with packages; [
      hindent
      hasktags
      stylish-haskell
      hlint
    ]);

  rust = latest.rustChannels.nightly.rust.override { 
    extensions = [ 
      "rust-src" 
    ]; 
  };

  # LaTeX installation
  texlive-collection = texlive.combine {
    inherit
      (texlive)
      scheme-basic
      collection-xetex
      collection-fontsrecommended;
  };

  dwm = pkgs.dwm.override {
    patches = [
      ./dwm/basic-setup.patch
    ];
  };


  xargo-drv = { rustPlatform, stdenv, fetchFromGitHub }:
    rustPlatform.buildRustPackage rec {
      name = "xargo-${version}";
      version = "v0.3.9";

      src = fetchFromGitHub {
        owner = "japaric";
        repo = "xargo";
        rev = "${version}";
        sha256 = "0p1bl41qkcr4bmrfkbz7n37hc6jakw5a2d9652zs8kiz5fk347c1";
      };

      depsSha256 = "19gn14r98qzmfkp5vkzkngm38l5fydik39q71fi5mryap30c15mb";

      meta = with stdenv.lib; {
        description = "Cargo which allows customization of std";
        homepage = "https://github.com/japaric/xargo";
        license = with licenses; [ mit /* or */ asl20 ];
        platforms = platforms.all;
      };
    };

  xargo-rustPlatform = recurseIntoAttrs (makeRustPlatform {
    rustc = latest.rustChannels.nightly.rust;
    cargo = latest.rustChannels.nightly.cargo;
  });

  xargo = xargo-drv { 
    rustPlatform = xargo-rustPlatform; 
    inherit stdenv fetchFromGitHub; 
  };

  ##############################################################################
  ## PACKAGE GROUPS
  ##############################################################################
  base = {
    xmodmap = xorg.xmodmap;
    inherit
      stdenv
      gcc
      ack
      ag
      global
      fzf
    
      ranger
      zathura
      w3m
      htop
      rofi
      custom-tmux
      gnupg1

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
      #xargo
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
      jdk
      leiningen
      boot;
  };

  elmTools = {
    elm-repl = unstable.elmPackages.elm-repl;
    elm-reactor = unstable.elmPackages.elm-reactor;
    elm-make = unstable.elmPackages.elm-make;
    elm-package = unstable.elmPackages.elm-package;
    elm-format = unstable.elmPackages.elm-format;
  };

  jsTools = {
    nodejs = unstable.nodejs;
    eslint = unstable.nodePackages.eslint;
    tern = unstable.nodePackages.tern;
  };

  opsTools = {
    inherit
      virtinst
      virt-viewer;
  };

  xmonadSupport = {
    xinput = xorg.xinput;
    xmobar = haskellPackages.xmobar;
    inherit
      rxvt_unicode
      maim
      slop
      xclip
      i3lock;
  };

  latexTools = {
    inherit
      texlive-collection;
  };

  sipTools = {
    inherit
      sipp;
  };

  nxe-tools = {
    nxe-build-image = nxe.tools.nxe-build-image;
  };
  
  myTools = {
    csv = obelisk.miniTools.csv;
  };

  others = {
    idris = haskellPackages.idris;
    inherit
      weechat

      firefox
      qutebrowser
      gtypist;
  };

in
################################################################################
## EXPOSED ATTRIBUTES
################################################################################
(base
// haskellTools
// rustTools
// scalaTools
// clojureTools
// elmTools
// jsTools
// opsTools
// xmonadSupport
// latexTools
// myTools
// sipTools
// others
// nxe-tools)

