let $LANG = 'en'
filetype indent plugin on
syntax on
set hidden        " allow buffer switching if there are changes

" disable some vim-polygot packages to prevent conflicts

call plug#begin('~/.local/share/nvim/plugged')
" Basic syntax etc for most languages
Plug 'sheerun/vim-polyglot'

" commenting
Plug 'tpope/vim-commentary'

" simple pretty statusline
Plug 'itchyny/lightline.vim'
Plug 'daviesjamie/vim-base16-lightline'

" base16 colorschemes
Plug 'chriskempson/base16-vim'

" opening files etc.
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" IntelliSense
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" run tests easily
Plug 'janko-m/vim-test'

" git stuff
Plug 'tpope/vim-fugitive'

call plug#end()

let test#python#runner = 'pytest'      " Always use pytest to run python tests

" use tab for completion
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

let base16colorspace=256

" use base16-shell to create the colorscheme file
source ~/.vimrc_background

let g:lightline = {
      \ 'colorscheme': 'base16'
      \ }

if has('nvim')
  set termguicolors
else
  set t_Co=256
end

set number
set relativenumber
set mouse=""

set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2

set encoding=utf-8

set list                  " show invisible characters
set listchars=tab:>\ ,trail:-,nbsp:+

set hlsearch
set incsearch
set ignorecase
set smartcase

" folding
set foldmethod=indent
set foldlevelstart=1
set nofoldenable          " do not fold automatically

set exrc                  " use extra rc file if found

let mapleader=" "

nnoremap <esc><esc> :noh<CR>

" use "very magic" mode for searching
nnoremap / /\v
vnoremap / /\v

" vim-test
nnoremap <silent> <leader>t :TestNearest<CR>
nnoremap <silent> <leader>T :TestFile<CR>
nnoremap <silent> <leader>a :TestSuite<CR>
nnoremap <silent> <leader>l :TestLast<CR>
nnoremap <silent> <leader>g :TestVisit<CR>

" fzf bindings
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>

" swap to previous buffer with ,,
nnoremap <leader><leader> <C-^>

" get fzf files with ag, it will use gitignore to hide files
" we also want to see dotfiles, so use --hidden
" note that .git needs to be added to .agignore in this case
let $FZF_DEFAULT_COMMAND = 'ag -g "" --hidden --ignore-dir node_modules'

" paste from yank register
nnoremap <leader>p "0p
nnoremap <leader>P "0P
