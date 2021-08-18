call plug#begin(stdpath('data') . '/plugged')

Plug 'dracula/vim', {'name': 'dracula'} " dracula color theme
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'Yggdroot/indentLine'  " indentLine plugin
Plug 'stautob/vim-fish'  " fish support for vim
Plug 'tpope/vim-fugitive'  " git stuff
Plug 'tpope/vim-surround'  " quoting and parenthesizing plugin
Plug 'jiangmiao/auto-pairs'  " quote pairs and other neat stuff
Plug 'https://github.com/tpope/vim-commentary'
" Plug 'vim-airline/vim-airline'  " airline plugin for status bar
Plug 'itchyny/lightline.vim'  " airline was throwing shitty errors so yeah.
Plug 'mbbill/undotree'  " undo tree
Plug 'bling/vim-bufferline'  " buffer line
Plug 'tpope/vim-fugitive'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'airblade/vim-gitgutter'
Plug 'majutsushi/tagbar'
if has('python')
	Plug 'laurentgoudet/vim-howdoi'
endif
Plug 'wakatime/vim-wakatime'  " wakatime for vim
Plug 'SuneelFreimuth/vim-gemtext'

" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'hrsh7th/nvim-compe'
Plug 'neovim/nvim-lspconfig'
call plug#end()

"let g:dracula_colorterm = 0
colorscheme dracula

" lightline
let g:lightline = {
			\ 'colorscheme': 'dracula',
			\ 'mode_map': {
			\ 'n' : 'NORM',
			\ 'i' : 'INS',
			\ 'R' : 'REP',
			\ 'v' : 'VIS',
			\ 'V' : 'VL',
			\ "\<C-v>": 'VB',
			\ 'c' : 'C',
			\ 's' : 'S',
			\ 'S' : 'SL',
			\ "\<C-s>": 'SB',
			\ 't': 'T',
			\ },
			\ 'component': {
			\         'tagbar': '%{tagbar#currenttag("[%s]", "")}',
			\ },
			\ 'component_function': {
			\   'fugitive': 'LightlineFugitive',
			\   'ctrlpmark': 'CtrlPMark',
			\ },
			\ 'active': {
			\       'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark', 'tagbar'] ],
			\ },
			\ }

function! LightlineFugitive()
  try
	if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*FugitiveHead')
	  let mark = ''  " edit here for cool mark
	  let branch = FugitiveHead()
	  return branch !=# '' ? mark.branch : ''
	endif
  catch
  endtry
  return ''
endfunction


" open current dir in nerdtree
noremap <Leader>nf :NERDTreeFind<CR>
" open nerdtree when openning a dir in vim
let NERDTreeWinSize = 20
" autocmd vimenter * NERDTree % | exe "normal \<c-w>l"
noremap <Leader>nt :NERDTreeToggle<CR>

" NerdTree Git plugin
let g:NERDTreeGitStatusIndicatorMapCustom = {
			\ "Modified"  : "M",
			\ "Staged"    : "A",
			\ "Untracked" : "?",
			\ "Renamed"   : "R",
			\ "Unmerged"  : "═",
			\ "Deleted"   : "D",
			\ "Dirty"     : "X",
			\ "Clean"     : "✔︎",
			\ 'Ignored'   : 'i',
			\ "Unknown"   : "??"
			\ }


" tagbar
nnoremap <leader>tt :TagbarToggle<CR>
let g:tagbar_width = 20

" source $HOME/.config/nvim/coc.vim
lua require('lsp')
lua require('autocomplete')
