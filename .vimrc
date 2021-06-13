set nocompatible

""""""""""""""""
" Vundle Plugins
""""""""""""""""
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'dracula/vim', {'name': 'dracula'}  " dracula color theme
Plugin 'stautob/vim-fish'                  " fish support for vim
Plugin 'jiangmiao/auto-pairs'              " auto insert and del quotes in pairs
Plugin 'tpope/vim-surround'                " quoting and parenthesizing plugin
" Plugin 'preservim/nerdcommenter'           " commenting made simple
Plugin 'tpope/vim-commentary'
Plugin 'bling/vim-bufferline'              " buffer line
Plugin 'preservim/nerdtree'                " NERDTree :)



call vundle#end()
filetype plugin indent on
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
" see :h vundle for more details or wiki for FAQ

" end of vundle stuff


""""""""""""""""""
" General settings
""""""""""""""""""
colorscheme dracula

let mapleader=";" " setting leader key to ';'
set number
syntax on
set mouse=a " allow mouse for all
set undofile " save undo to a file
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
set wrap  " dont wrap
set incsearch  " find next match while typing search
set linebreak  " wrap lines at convenient points
set scrolloff=6  " screen lines to keep above and below cursor
set sidescrolloff=8  " screen columns to keep on left and right of cursor
set confirm  " display confirmation when closing unsaved file
set encoding=utf-8  " set encoding with Unicode
set lazyredraw  " don't redraw when exe macros
set showmatch  " match brackets when text indecator is over it
set mat=2  " how mny tenths of second to blink when matching brackets

set laststatus=2  " show status line
set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L\ \|\ col\ %c)

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" fold settings
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=manual
set foldcolumn=2


"set the swp, backup and undo settings
set noswapfile
set nobackup nowritebackup
set undodir=~/.vim/undodir
set undofile

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
	set wildignore+=.git\*,.hg\*,.svn\*
else
	set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" highlight extra whitespace
match ErrorMsg '\s\+$'

"""""""""""
" Mappings
""""""""""
" enabling clipboard syncing in WSL
source $HOME/iswsl.vim
if IsWSL()
	let s:clip = '/mnt/c/Windows/System32/clip.exe'
	if executable(s:clip)
		augroup WSLYank
			autocmd!
			autocmd TextYankPost * call system('echo '.shellescape(join(v:event.regcontents, "\<CR>")).' | '.s:clip)
		augroup END
	end
	map <silent> "=p :r !powershell.exe -Command Get-Clipboard<CR>
	map! <silent> <C-r>= :r !powershell.exe -Command Get-Clipboard<CR>
	noremap "+p :exe 'norm a'.system('/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command Get-Clipboard')<CR>
endif

" leader mappings
nnoremap <Leader>rn :set relativenumber!<CR>
nnoremap <Leader>z zR
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :wqa<CR>
nnoremap <Leader>q :qa<CR>
noremap <Leader>n :NERDTreeToggle %<CR>
" no highlight - when finished search
nnoremap <Leader>nh :noh<CR>
" paste shortcut below
" must be recursive bcus "+p is aldy mapped above (in WSLYank - paste)
nmap <Leader>pa "+p
nnoremap <Leader>rg :registers<CR>

" other mappings
" dot command in visual mode
vnoremap . :normal.<CR>
nnoremap Q :q<CR>
" move visual selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

if IsWSL()
	execute "set <A-j>=\ej"
	execute "set <A-k>=\ek"
	execute "set <A-J>=\eJ"
	execute "set <A-K>=\eK"
endif

nnoremap <A-j> :m+1<CR>==
nnoremap <A-k> :m-2<CR>==

nnoremap <A-J> :t.<CR>==
nnoremap <A-K> :t.-1<CR>==

" Visual mode pressing * or # searches for the current selection
"  Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>"

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
map <leader>t<leader> :tabnext
map <leader>bn :bnext<cr>
map <leader>bp :bprev<cr>

noremap <Leader>n :NERDTreeToggle %<CR>


""""""""""""""""
" other settings
""""""""""""""""
" setting the shell for fish to sh
"if &shell =~# 'fish$'
"	set shell=bash
"endif

