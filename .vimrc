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
set laststatus=2 "for lightline"
"set guifont=FiraCode\ Nerd\ Font\ Mono\ 10
set guifont=Fira\ Code\ iScript\ 10

"### syntastic plugin config ###
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"if (has("termguicolors"))
"  set termguicolors
"endif

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
Plug 'jiangmiao/auto-pairs'
Plug 'Vimjas/vim-python-pep8-indent'
Plug 'dense-analysis/ale'
Plug 'vimwiki/vimwiki'

"---nerdtree 
Plug 'preservim/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'preservim/nerdtree' |
            \ Plug 'Xuyuanp/nerdtree-git-plugin'
"-------------------------------------------------
Plug 'airblade/vim-gitgutter'
Plug 'ryanoasis/vim-devicons'
Plug 'itchyny/lightline.vim'
Plug 'vifm/vifm.vim'
Plug 'francoiscabrol/ranger.vim'
Plug 'preservim/nerdcommenter' "to give a comment
Plug 'junegunn/fzf.vim' "fuzzy finder
Plug 'adelarsq/vim-matchit'
Plug 'vim-python/python-syntax'
"===== autocomplete & snippets 
Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'prabirshrestha/vim-lsp'
"============================================
Plug 'ap/vim-css-color'
Plug 'chrisbra/colorizer'
Plug 'neovimhaskell/haskell-vim'
Plug 'vim-syntastic/syntastic'
Plug 'artanikin/vim-synthwave84' 
Plug 'dylanaraps/wal.vim' "--colorscheme
"Plug 'bitc/vim-hdevtools'
"### Vim language packs ###
Plug 'sheerun/vim-polyglot'
"----------- Snippets - Vsnip -----------
"Plug 'hrsh7th/vim-vsnip'
"Plug 'hrsh7th/vim-vsnip-integ'
"shell snipp
"Plug 'yousefvand/shellman'
"Plug 'rafamadriz/friendly-snippets'
Plug 'voldikss/vim-floaterm'
Plug 'alpertuna/vim-header'
Plug 'tweekmonster/startuptime.vim'
call plug#end()

let g:ale_linters = {
    \ 'sh': ['language_server'],
    \ }

if executable('bash-language-server')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'bash-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'bash-language-server start']},
        \ 'allowlist': ['sh'],
        \ })
endif

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
let g:lightline = { 'colorscheme': 'deepspace' }
colorscheme deep-space 

"colorscheme elflord
"colorscheme wal 

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

" ---- Nerdtree syntax highlighting Configuration ----
"let g:WebDevIconsDisableDefaultFolderSymbolColorFromNERDTreeDir = 1
"let g:WebDevIconsDisableDefaultFileSymbolColorFromNERDTreeFile = 1

" ------ Nerdtree git plugin --------------
let g:NERDTreeGitStatusIndicatorMapCustom = {
                \ 'Modified'  :'✹',
                \ 'Staged'    :'✚',
                \ 'Untracked' :'✭',
                \ 'Renamed'   :'➜',
                \ 'Unmerged'  :'═',
                \ 'Deleted'   :'✖',
                \ 'Dirty'     :'✗',
                \ 'Ignored'   :'☒',
                \ 'Clean'     :'✔︎',
                \ 'Unknown'   :'?',
                \ }

let g:NERDTreeGitStatusUseNerdFonts = 1 " you should install nerdfonts by yourself. default: 0
let g:NERDTreeGitStatusShowIgnored = 1 " a heavy feature may cost much more time. default: 0
let g:NERDTreeGitStatusUntrackedFilesMode = 'all' " a heave feature too. default: normal
"let g:NERDTreeGitStatusGitBinPath = '/your/file/path' " default: git (auto find in path)
"let g:NERDTreeGitStatusShowClean = 1 " default: 0
"let g:NERDTreeGitStatusConcealBrackets = 1 " default: 0

"================================  Coc Snippets ========== =========================== 

" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

" Use <leader>x for convert visual selected code to snippet
xmap <leader>x  <Plug>(coc-convert-snippet)

"Make <tab> used for trigger completion, completion confirm, snippet expand and jump like VSCode.

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

"inoremap <silent><expr> <TAB>
"      \ pumvisible() ? coc#_select_confirm() :
"      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
"      \ <SID>check_back_space() ? "\<TAB>" :
"      \ coc#refresh()


inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

"inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

"let g:coc_snippet_next = '<tab>'
"=======================================================================================================

"----------------------------- Vim VSNIP config ----------------------------------
"NOTE: You can use other key to expand snippet.
" Expand
"imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
"smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

"" Expand or jump
"imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
"smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

"" Jump forward or backward
"imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
"smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
"imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
"smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

"" Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
"" See https://github.com/hrsh7th/vim-vsnip/pull/50
"nmap        s   <Plug>(vsnip-select-text)
"xmap        s   <Plug>(vsnip-select-text)
"nmap        S   <Plug>(vsnip-cut-text)
"xmap        S   <Plug>(vsnip-cut-text)

"" If you want to use snippet for multiple filetypes, you can `g:vsnip_filetypes` for it.
"let g:vsnip_filetypes = {}
""let g:vsnip_filetypes.javascriptreact = ['javascript']
""let g:vsnip_filetypes.typescriptreact = ['typescript']
"===================================================================================================

"------------------ FLoatTerm keymap ------------------------------
let g:floaterm_keymap_new    = '<F7>'
let g:floaterm_keymap_prev   = '<F8>'
let g:floaterm_keymap_next   = '<F9>'
let g:floaterm_keymap_toggle = '<F10>'

"================ Vim Header =======================
let g:header_auto_add_header = 0
let g:header_field_author = 'Steven Agustinus'
let g:header_field_author_email = "steven87.ags@gmail.com"
let g:header_field_license_id = 'MIT'
nnoremap <leader>h :AddHeader<CR>
"add license
"=== GNU GP License v3 ( GPL )===
nnoremap <leader>lg :AddGNULicense<CR> 
"=== MIT License ===
nnoremap <leader>lm :AddMITLicense<CR>


