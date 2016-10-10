" Set up Vundle
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'molokai'

call vundle#end()

filetype plugin indent on

" Turn on syntax highlighting and use molokai
syntax on
colorscheme molokai

" Set relative line numbering
set relativenumber


" Powerline stuff

:set background&

" Start up powerline
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" Activate powerline's status bar, and set vim to use
" 256 colours.
set laststatus=2
set t_Co=256
