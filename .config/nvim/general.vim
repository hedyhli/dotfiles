""""""""""""""""""
" General settings
" """"""""""""""

set number
set mouse=a " allow mouse for all
set hlsearch " highlight search
set showcmd  " show incomplete commands
set wildmenu  " command line's tab complete in a menu
set cursorline  " highlight cursor line
set noerrorbells  " no beeps
set visualbell  " flash screen instead
set title  " set window title to file name
set autoindent
set softtabstop=2  " indent by 2 spaces when with tab
set tabstop=4  " show existing tab with 4 spaces width
set shiftwidth=4
set incsearch  " find next match while typing search
set scrolloff=6  " screen lines to keep above and below cursor
set sidescrolloff=8  " screen columns to keep on left and right of cursor
set confirm  " display confirmation when closing unsaved file
set encoding=utf-8  " set encoding with Unicode
set showmatch  " match brackets when text indecator is over it
set mat=2  " how mny tenths of second to blink when matching brackets
set inccommand=nosplit  " neovim only

" settings required from coc
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

set termguicolors


set laststatus=2  " show status line
"set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L\ \|\ col\ %c)

set whichwrap+=<,>,h,l

"" fold settings
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual
set foldcolumn=2


" set the swp, backup and undo settings
set noswapfile
set nobackup nowritebackup
set undodir=~/.local/share/nvim/undodir/
set undofile
"
"" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
	set wildignore+=.git\*,.hg\*,.svn\*
else
	set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

" Return to last edit position when opening files (You want
" this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"au ColorScheme * hi Normal ctermbg=None

"" highlight extra whitespace
match ErrorMsg '\s\+$'

" setting the shell for fish to sh
"if &shell =~# 'fish$'
"	set shell=bash
"endif
"

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

