let $LANG = 'en'
filetype indent plugin on
syntax on
set hidden        " allow buffer switching if there are changes

" disable some vim-polygot packages to prevent conflicts
let g:polyglot_disabled = ['go']

call plug#begin('~/.local/share/nvim/plugged')
" language specific plugins
Plug 'fatih/vim-go'

" Basic syntax etc for most languages
Plug 'sheerun/vim-polyglot'

" commenting
Plug 'tpope/vim-commentary'

" gruvbox
Plug 'gruvbox-community/gruvbox'
"
" simple pretty statusline
Plug 'itchyny/lightline.vim'

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

colorscheme gruvbox

let g:lightline = {
      \ 'colorscheme': 'gruvbox'
      \ }

let g:fzf_layout = { 'down': '50%' }

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


" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif


" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>F  <Plug>(coc-format-selected)
nmap <leader>F  <Plug>(coc-format-selected)
