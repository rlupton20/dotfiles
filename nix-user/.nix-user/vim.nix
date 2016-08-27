{pkgs ? import <nixpkgs> {} }:

with pkgs; rec {
  custom-vim = vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = ''
      syntax on
      colorscheme molokai
    '';
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
    vimrcConfig.vam.pluginDictionaries = [
      { names = [
        "molokai"
      ]; }
      ];
  };

}
