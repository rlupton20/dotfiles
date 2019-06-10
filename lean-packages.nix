let
  _licenses = (import <nixpkgs> {}).stdenv.lib.licenses;

  pkgs = import <nixpkgs> {
    config = {
      blacklistedLicenses = with _licenses; [
        unfreeRedistributableFirmware
      ];
    };
  };

in with import <nixpkgs> {}; let
  base = {
    inherit
      stdenv
      gcc9
      global
      groff
      w3m
      zathura;
  };

  prog = {
    inherit
      mitscheme;  # Scheme
  };
in
(base // prog)
