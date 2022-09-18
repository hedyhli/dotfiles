" ==========================
" Plugins and their settings
" ==========================
"
" === Plugin declarations ===
call plug#begin(stdpath('data') . '/plugged')

Plug 'dracula/vim', {'name': 'dracula'} " dracula color theme (THE most important plugin, yes)
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'  " Show git statuses in NERDTree
Plug 'Yggdroot/indentLine'          " No idea what it is (too lazy to check);
                                    " something I had since a long time ago but
                                    " didn't want to remove
Plug 'tpope/vim-fugitive'           " git stuff
Plug 'jreybert/vimagit'             " more git stuff
Plug 'tpope/vim-surround'           " quoting and parenthesizing plugin
" This plugin below is really good, but whenever I'm on a commented line,
" press o, press backspace, the line below is joined up above. You won't
" believe how long it took me to debug this problem and finally realize it's
" because of this plugin.
" Plug 'jiangmiao/auto-pairs'
"
" So now I'm using these instead:
" 1. nvim-autopairs - extremely customizable, written in lua
" 2. vim-autoclose - fallback if nvim < v0.5 (the installation might not have
" the required lua files or something)
if has('nvim-0.5')
  if has('nvim-0.7')
    Plug 'windwp/nvim-autopairs'
  else
    Plug 'windwp/nvim-autopairs', { 'commit': 'b7672cd' }
  endif
  " Best IDE autocomplete setup ever
  Plug 'neovim/nvim-lspconfig', { 'commit': '10ad0cca1b' }
  Plug 'hrsh7th/nvim-compe'
else
  Plug 'townk/vim-autoclose'
  " Picking the right LSP completion method, see bottom of file for more
  if has('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
  endif
endif
Plug 'tpope/vim-commentary'
Plug 'itchyny/lightline.vim'    " airline was throwing shitty errors so yeah.
                                " Pretty and customizable status bar,
                                " for components see below; TODO:
                                " switch to galaxy line in the future
" Commented out because I realized I never used it (lol)
" Plug 'mbbill/undotree'  " undo tree
Plug 'bling/vim-bufferline'     " buffer line (one of my most used plugins!)
Plug 'ctrlpvim/ctrlp.vim'       " Quickly find a fine with fuzzy find; TODO: use fzf instead
Plug 'airblade/vim-gitgutter'   " Show git diff overview stuff in the left column
Plug 'majutsushi/tagbar'        " Quickly jump to a symbol in buffer (one of my most used omg!)
" TODO: Figure out a way of using both endwise and auto pair

" === File type, syntax, or language helper plugins ===
" gemtext syntax highlighting; I know there are more popular alternatives but
" this is the best IMO
Plug 'https://git.sr.ht/~torresjrjr/gemini.vim' , { 'for': 'gemini' }
Plug 'cespare/vim-toml'                         , { 'for': 'toml' }
Plug 'blankname/vim-fish'                       , { 'for': 'fish' }
Plug 'hedyhli/vim-bun'                          , { 'for': 'bun' }
Plug 'https://git.rawtext.club/slope-lang/slope-vim-syntax' , { 'for': 'slope' }
Plug 'mzlogin/vim-markdown-toc'                 , { 'for': 'markdown' }

" beware, this<tab>
" Plug 'github/copilot.vim'

call plug#end()
" Plugin declarations ends here


colorscheme dracula  " THE most important nvim configuration


" === Lightline Settings ===
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
        \   'tagbar': '%{tagbar#currenttag("[%s]", "")}',
      \ },
      \ 'component_function': {
      \   'fugitive': 'LightlineFugitive',
      \   'diagnosticscount': 'LightlineDiagnostics',
      \ },
      \ 'component_type': {
        \   'diagnosticscount': 'error',
      \ },
      \ 'active': {
        \       'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['tagbar'] ],
        \		'right': [ [ 'diagnosticscount' ], ['percent'], ['fileformat', 'fileencoding', 'filetype'] ]
        \ },
      \ }

function! LightlineFugitive()
  " Referenced from Lightline docs; I'm not 100% what this does but seems like
  " it just grabs the current git branch
  try
	if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*FugitiveHead')
	  let mark = ''  " I'm not sure what this mark does either
	  let branch = FugitiveHead()
	  return branch !=# '' ? mark.branch : ''
	endif
  catch
  endtry
  return ''
endfunction

function! LightlineDiagnostics()
  " Grabs the current buffer's diagnostics count and displays it in the
  " format 'E:x W:x'
  " If both errors and warnings are 0 then don't display anything
  try
    " TODO: if we don't have nvim-0.5 then call some CoC function
    if has('nvim-0.6')
      let errors = luaeval('vim.diagnostic.get_count(0, [[Error]])')
      let warnings = luaeval('vim.diagnostic.get_count(0, [[Warning]])')
    else
      let errors = luaeval('vim.lsp.diagnostic.get_count(0, [[Error]])')
      let warnings = luaeval('vim.lsp.diagnostic.get_count(0, [[Warning]])')
    endif
    if errors == 0 && warnings == 0
      return ''
    else
      return "E:" . errors . " W:" . warnings
    endif
  catch
  endtry
  return ''
endfunction


" === Other plugins settings ===

let NERDTreeWinSize = 20
" Open current dir in nerdtree
noremap <Leader>nf :NERDTreeFind<CR>
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

" === LSP ===
if has("nvim-0.5")
  lua require('autopair')
  lua require('lsp')
  lua require('complete')

  " Set completeopt to have a better completion experience
  set completeopt=menuone,noinsert,noselect,preview

  " compe mappings for nvim-autopairs
  inoremap <silent><expr> <C-Space> compe#complete()
  " endwise remaps <CR> though so it doesn't work
  inoremap <silent><expr> <CR>      compe#confirm(luaeval("require 'nvim-autopairs'.autopairs_cr()"))
  inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
  inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

  " Autoclose completion popup when completion is done
  " TODO: Doesn't seem to work though?
  " Just press C-g I guess (defined in ./lua/autocomplete.lua)
  augroup complete_hide_popup
    autocmd! CompleteDone * if pumvisible() == 1 | pclose | endif
  augroup END
else
  if has('node')
    source $HOME/.config/nvim/coc.vim
  endif
endif

