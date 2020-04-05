" Plugins
call plug#begin('~/.vim/plugged')

Plug 'arcticicestudio/nord-vim'
Plug 'vim-airline/vim-airline'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'rust-lang/rust.vim', {'do': 'cargo +nightly install racer -f; rustup component add rls rust-analysis rust-src'}
Plug 'neovimhaskell/haskell-vim'
Plug 'leafgarland/typescript-vim'
Plug 'pangloss/vim-javascript'
Plug 'posva/vim-vue'
Plug 'JulesWang/css.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'tbastos/vim-lua'
Plug 'dense-analysis/ale'
Plug 'vim-syntastic/syntastic'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'rhysd/vim-grammarous'
Plug 'preservim/nerdtree'
Plug 'junegunn/fzf' 
Plug 'junegunn/fzf.vim'

call plug#end()

set nocompatible
set ttyfast

" Visual preferences
set number
set showcmd
set background=dark
syntax enable

colorscheme nord

" Nerdtree preferences
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif

map <C-n> :NERDTreeToggle<CR>

" <3 fzf
let g:fzf_command_prefix = 'Fzf'
let g:fzf_layout=  { 'down': '~20%' }

map <C-b> :FzfFiles<CR>

" Vim hotkeys
map H ^
map L $

inoremap { {}<Left>
inoremap [ []<Left>


set backspace=indent,eol,start

" LaTeX hotkeys
map I :! pdflatex %<CR><CR>
map R :! pdflatex % && pkill -HUP mupdf<CR><CR>
map S :! mupdf $(echo % \| sed 's/tex$/pdf/') & disown<CR><CR>

" Panes, but better
set splitright
set splitbelow

nnoremap <C-H> <C-W><C-H>
nnoremap <C-J> <C-W><C-j>
nnoremap <C-K> <C-W><C-k>
nnoremap <C-L> <C-W><C-l>

" Syntax fixing with ALE
let b:ale_fixers = {'javascript': ['eslint'], 'vue': ['eslint']}
let g:ale_fix_on_save = 1

" Styling for various languages
filetype plugin indent on

augroup filetypedetect
  autocmd FileType rust      setlocal expandtab    shiftwidth=4 tabstop=4
  autocmd FileType vue       setlocal expandtab    shiftwidth=2 tabstop=2
  autocmd FileType json      setlocal expandtab    shiftwidth=2 tabstop=2
  autocmd FileType go        setlocal noexpandtab  shiftwidth=4 tabstop=4
  autocmd FileType scss      setlocal expandtab    shiftwidth=2 tabstop=2
  autocmd FileType css       setlocal expandtab    shiftwidth=2 tabstop=2
  autocmd FileType java      setlocal noexpandtab  shiftwidth=4 tabstop=4
  autocmd FileType js        setlocal expandtab    shiftwidth=2 tabstop=2
  autocmd FileType tex       setlocal noexpandtab  shiftwidth=4 tabstop=4
  autocmd FileType haskell   setlocal expandtab    shiftwidth=4 tabstop=4

" Prefer rust-analyzer over syntastic
let g:syntastic_mode_map = { 'passive_filetypes': ['rust'] }
