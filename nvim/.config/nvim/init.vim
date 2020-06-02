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

" pip install neovim in these virtualens
let g:python3_host_prog=expand("~/.local/share/virtualenvs/neovim3/bin/python")

set completeopt-=preview               " do not open window for function params etc
let g:jedi#completions_enabled = 0     " do not use vim-jedi completions
let g:deoplete#enable_at_startup = 1

let g:ale_linters = {
      \  'python': ['flake8'],
      \  'javascript': ['eslint'],
      \  'scala': [],
      \}
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_text_changed = 'normal'
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

let mapleader=","

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

nnoremap <C-n> :NERDTreeToggle<CR>

" fold with space
nnoremap <Space> za

" evaluate ruby code in editor
nmap <buffer> <F4> <Plug>(xmpfilter-run)
xmap <buffer> <F4> <Plug>(xmpfilter-run)
imap <buffer> <F4> <Plug>(xmpfilter-run)
nmap <buffer> <F3> <Plug>(xmpfilter-mark)
xmap <buffer> <F3> <Plug>(xmpfilter-mark)
imap <buffer> <F3> <Plug>(xmpfilter-mark)

" ALE bindings
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" get fzf files with ag, it will use gitignore to hide files
" we also want to see dotfiles, so use --hidden
" note that .git needs to be added to .agignore in this case
let $FZF_DEFAULT_COMMAND = 'ag -g "" --hidden --ignore-dir node_modules'

" paste from yank register
nnoremap <leader>p "0p
nnoremap <leader>P "0P
