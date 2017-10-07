{pkgs ? import <nixpkgs> {}}:
let
  custom_vim_configurable = with pkgs;
    vimUtils.makeCustomizable (vim_configurable.override { python = python3; });

  customPlugins.vim-latex-suite = pkgs.vimUtils.buildVimPlugin {
    name = "vim-latex-suite";
    src = pkgs.fetchFromGitHub {
      owner = "vim-latex";
      repo = "vim-latex";
      rev = "v1.9.0";
      sha256 = "1zym13sx7fh6pcklsdfv3yab2km41picf2c753swva12grjih5an";
    };
  };
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

      filetype plugin on

      let g:tex_flavor='latex'
    '';
    vimrcConfig.vam.knownPlugins = pkgs.vimPlugins // customPlugins;
    vimrcConfig.vam.pluginDictionaries = [
      { names = [
        "molokai"
        "vim-airline"
        "vim-airline-themes"
        "youcompleteme"
        "Syntastic"
        "vinegar"
        "vim-latex-suite"
      ]; }
    ];
  };

  custom-neovim = neovim.override {
    configure = {
      customRC = ''
	colorscheme molokai
        set relativenumber

        let g:airline_powerline_fonts = 1
        let g:airline_theme='powerlineish'
        set laststatus=2
      '';
      vam.knownPlugins = pkgs.vimPlugins;
      vam.pluginDictionaries = [
        { names = [
	  "molokai"
	  "vim-airline"
	  "vim-airline-themes"
	  "vinegar"
	  ];
	}
      ];
    };
  };
}
