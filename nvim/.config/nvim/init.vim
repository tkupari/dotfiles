let $LANG = 'en'
filetype indent plugin on
syntax on
set hidden        " allow buffer switching if there are changes

" disable some vim-polygot packages to prevent conflicts
let g:polyglot_disabled = ['elixir', 'elm', 'ruby']

call plug#begin()
" Basic syntax etc for most languages
Plug 'sheerun/vim-polyglot'

" language plugins
Plug 'ElmCast/elm-vim'
Plug 'elixir-editors/vim-elixir'
Plug 'vim-ruby/vim-ruby'
Plug 't9md/vim-ruby-xmpfilter'
Plug 'tpope/vim-rails'
Plug 'tweekmonster/django-plus.vim'

" simple pretty statusline
Plug 'itchyny/lightline.vim'
Plug 'daviesjamie/vim-base16-lightline'

" base16 colorschemes
Plug 'chriskempson/base16-vim'

" opening files etc.
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'

" autocomplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'slashmili/alchemist.vim'
Plug 'davidhalter/jedi-vim'
Plug 'zchee/deoplete-jedi'

" run tests easily
Plug 'janko-m/vim-test'

" git stuff
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki', { 'branch': 'dev' }

" linting
Plug 'w0rp/ale'
call plug#end()

" pip install neovim in these virtualens
let g:python3_host_prog=expand("~/.virtualenvs/neovim3/bin/python")
let g:python_host_prog=expand("~/.virtualenvs/neovim/bin/python")

set completeopt-=preview               " do not open window for function params etc
let g:jedi#completions_enabled = 0     " do not use vim-jedi completions
let g:deoplete#enable_at_startup = 1

let g:ale_linters = {
      \  'python': ['flake8'],
      \  'javascript': ['eslint'],
      \}
let test#python#runner = 'pytest'      " Always use pytest to run python tests
let test#python#pytest#options = '--reuse-db'

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

set clipboard=unnamedplus " Use system clipboard

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

" get fzf files with ag, it will use gitignore to hide files
" we also want to see dotfiles, so use --hidden
" note that .git needs to be added to .agignore in this case
let $FZF_DEFAULT_COMMAND = 'ag -g "" --hidden'
