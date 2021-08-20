" ================
" General settings
" ================

set number
set mouse=a         " allow mouse for all; TODO: I rarely use mouse (duh) so maybe remove this
set cursorline      " highlight current cursor line (this is SO GOOD)
set showcmd         " show incomplete commands
set hlsearch        " highlight search
set wildmenu        " command line's tab complete in a menu
set noerrorbells    " no beeps please
set visualbell      " flash screen instead
set title           " set window title to file name
set incsearch       " incrementally find next match while typing search
set scrolloff=6     " screen lines to keep above and below cursor
set sidescrolloff=8  " screen columns to keep on left and right of cursor
set confirm         " display confirmation when closing unsaved file
set encoding=utf-8  " set encoding with Unicode
set showmatch       " match brackets when cursor is over it
set mat=2           " how many tenths of second to blink when matching brackets
set inccommand=nosplit  " neovim only

" Indenting
set autoindent      " Keep indentation from previous line when RET (I think)
set expandtab       " AIUI, tab -> spaces
set softtabstop=4   " indent by 2 spaces with tab
set tabstop=4       " show existing tabs with 4 spaces width
set shiftwidth=4    " Put or remove 4 spaces with using < and >
set smarttab        " Delete spaces at tabstop width
set copyindent      " Copy indentation from previous line

" Settings required from coc
" Still keeping this if we're using lua+lsp instead of CoC because why not
set hidden
set cmdheight=2
set updatetime=300
set shortmess+=c
" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Bunch of shit really, spent *hours* trying to get tmux + nvim true colors to work
" TODO: check has('termguicolors') and set a env var or something
set termguicolors

" This is to fix the issue of when you are on a commented line (like # ...),
" then you press o, and you want to remove the `# ` so you hit backspace, and
" suddenly the line after the cursor is joined up to the current line.


" Let <left> <right> and h, l keys be able to move to previous or next line
" when currently on start or end of line respectively
set whichwrap+=<,>,h,l

" fold settings
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual
set foldcolumn=2

" Set the swp, backup and undo settings
set noswapfile
set nobackup nowritebackup
set undodir=~/.local/share/nvim/undodir/
set undofile

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
  set wildignore+=.git\*,.hg\*,.svn\*
else
  set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

" Return to last edit position when opening files
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" highlight trailing whitespace
match ErrorMsg '\s\+$'

" setting the shell for fish to bash
"if &shell =~# 'fish$'
"	set shell=bash
"endif
" commented because without it things still seems to work

" IsWSL function sourced in functions.vim, declared in ~/iswsl.vim
" I think this is a neovim-only thing, +1 for neovim :smirk:
if IsWSL()
  let g:clipboard = {
        \   'name': 'WSLClip',
        \   'copy': {
          \      '+': 'clip.exe',
          \      '*': 'clip.exe',
          \	},
        \   'paste': {
          \      '+': 'pbpaste',
          \      '*': 'pbpaste',
          \	},
          \   'cache_enabled': 0,
        \ }
endif

