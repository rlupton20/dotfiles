{pkgs ? import <nixpkgs> {}}:
let
  custom_vim_configurable = with pkgs;
    vimUtils.makeCustomizable (vim_configurable.override { python = python3; });
in
with pkgs; rec {
  custom-vim = custom_vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = ''
      syntax on
      colorscheme molokai

      set relativenumber

      let g:airline_powerline_fonts = 1
      let g:airline_theme='powerlineish'
      set laststatus=2
    '';
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins;
    vimrcConfig.vam.pluginDictionaries = [
      { names = [
        "molokai"
        "vim-airline"
        "vim-airline-themes"
        "youcompleteme"
        "Syntastic"
      ]; }
    ];
  };
}
