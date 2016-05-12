:set background&

" Start up powerline
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup

" Activate powerline's status bar, and set vim to use
" 256 colours.
set laststatus=2
set t_Co=256
