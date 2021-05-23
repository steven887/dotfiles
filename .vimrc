" #######################################################
"      _____ ___                                        #
"     / ___//   |    # My Personal Config               #
"     \__ \/ /| |    # https://gitlab.com/stevenags     # 
"    ___/ / ___ |    # Author : steven Agustinus        #
"   /____/_/  |_|                                       #
"                                                       # 
" #######################################################


syntax on
filetype plugin on
set nocompatible
set clipboard=unnamedplus
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set nu
set nowrap
set smartcase
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch
set termguicolors
"set t_Co=256
set encoding=UTF-8
set laststatus=2
set guifont=FiraCode\ Nerd\ Font\ Mono\ 10

"### syntastic plugin config ###
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
"### end ###

"install vim Plug
"curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

call plug#begin()
Plug 'nathanaelkane/vim-indent-guides'
"---colorschemes
Plug 'morhetz/gruvbox'
Plug 'tyrannicaltoucan/vim-deep-space'
"-----------------
Plug 'joshdick/onedark.vim'
Plug 'Valloric/YouCompleteMe' "auto complete
Plug 'jiangmiao/auto-pairs'
Plug 'Vimjas/vim-python-pep8-indent'
"Plug 'dense-analysis/ale'
Plug 'vimwiki/vimwiki'
Plug 'preservim/nerdtree'
Plug 'ryanoasis/vim-devicons'
Plug 'itchyny/lightline.vim'
Plug 'vifm/vifm.vim'
Plug 'francoiscabrol/ranger.vim'
Plug 'preservim/nerdcommenter' "to give a comment
Plug 'junegunn/fzf.vim' "fuzzy finder
Plug 'adelarsq/vim-matchit'
Plug 'vim-python/python-syntax'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ap/vim-css-color'
Plug 'chrisbra/colorizer'
Plug 'neovimhaskell/haskell-vim'
Plug 'vim-syntastic/syntastic'
Plug 'artanikin/vim-synthwave84'
"Plug 'bitc/vim-hdevtools'
"### Vim language packs ###
Plug 'sheerun/vim-polyglot'
call plug#end()


"ale plugin
"let g:ale_fixers = {
"\   '*': ['remove_trailing_lines', 'trim_whitespace'],
"\   'javascript': ['eslint'],
"\}
"let g:ale_fix_on_save = 1

"====================== ColorSchemes ======================
" ============= GRUVBOX  ===========
"let g:gruvbox_italic=1
"let g:gruvbox_termcolors=256
"let g:gruvbox_transparent_bg=1
"let g:gruvbox_contrast_dark='hard'
"set bg=dark
"colorscheme gruvbox
"===================================

" ============= Deep Space ========
set background=dark
let g:deepspace_italics=1
let g:airline_theme='deep_space'
let g:lightline = { 'colorscheme': 'deepspace' }
colorscheme deep-space
"=========================================================

"colorscheme elflord

set ts=4 sw=4 et
let g:indent_guides_start_level=2
let g:indent_guides_size=1
let g:AutoPairsFlyMode=1

"Re-binding esc key to kj
inoremap kj <Esc>

"set leader key to <space>
let mapleader = "\<space>"

"---colorizer
let g:colorizer_auto_color = 1
let g:colorizer_auto_filetype='css,html,haskell,yml '
"let g:colorizer_x11_names = 1
let g:colorizer_syntax = 1
"let g:colorizer_hex_pattern = ['#', '\%(\x\{3}\|\x\{6}\)', '']


"set leader key for vertical split
nnoremap <Leader>vs :vs<CR>
nnoremap <Leader>hs :sp<CR>
nnoremap <tab> <c-w>
nnoremap <tab><tab> <c-w><c-w>

"keybinding for NerdTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

"disabled arrow key in keyboard
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>

"-------- Haskell config for vim ------
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:ghc = "/usr/bin/ghc"

"----------------- syntastic plugin ---------------
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"-------------------hdevtools-----------------------
"let g:hdevtools_stack = 1
let g:hdevtools_options = '-g-fdefer-type-errors'

" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"


