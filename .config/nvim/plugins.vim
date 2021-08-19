" ==========================
" Plugins and their settings
" ==========================
"
" === Plugin declarations ===
call plug#begin(stdpath('data') . '/plugged')

Plug 'dracula/vim', {'name': 'dracula'} " dracula color theme (THE most important plugin, yes)
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'      " Show git statuses in NERDTree
Plug 'Yggdroot/indentLine'
Plug 'stautob/vim-fish'                 " fish support for vim
Plug 'tpope/vim-fugitive'               " git stuff
Plug 'tpope/vim-surround'               " quoting and parenthesizing plugin
Plug 'jiangmiao/auto-pairs'             " quote pairs and other neat stuff; TODO: switch to nvim-autopairs
Plug 'https://github.com/tpope/vim-commentary'
Plug 'itchyny/lightline.vim'            " airline was throwing shitty errors so yeah.
                                        " Pretty and customizable status bar,
                                        " for components see below
" Commented out because I realized I never used it (lol)
" Plug 'mbbill/undotree'  " undo tree
Plug 'bling/vim-bufferline'             " buffer line (one of my most used plugins!)
Plug 'ctrlpvim/ctrlp.vim'               " Quickly find a fine with fuzzy find; TODO: use fzf instead
Plug 'airblade/vim-gitgutter'           " Show git diff overview stuff in the left column
Plug 'majutsushi/tagbar'                " Quickly jump to a symbol in buffer (one of my most used omg!)
" Plug 'wakatime/vim-wakatime'            " Tracks my coding stats (because I like to look at the stats for fun)
Plug 'https://git.sr.ht/~torresjrjr/gemini.vim' " gemtext syntax highlighting; I know there are more
                                        " popular alternatives but this is the
                                        " best IMO

" Picking the right LSP completion method, see bottom of file for more
if has("nvim-0.5")
	Plug 'hrsh7th/nvim-compe'
	Plug 'neovim/nvim-lspconfig'
else
	Plug 'neoclide/coc.nvim', {'branch': 'release'}
endif
call plug#end()
" Plugin declarations ends here
" =============================

colorscheme dracula  " Probably THE most important nvim configuration ;-;

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
    let errors = luaeval('vim.lsp.diagnostic.get_count(0, [[Error]])')
    let warnings = luaeval('vim.lsp.diagnostic.get_count(0, [[Warning]])')
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
	lua require('lsp')
	lua require('autocomplete')

  " Autoclose completion popup when completion is done
  " TODO: Doesn't seem to work though?
  " Just press C-e I guess (defined in ./lua/autocomplete.lua)
  augroup complete_hide_popup
    autocmd! CompleteDone * if pumvisible() == 1 | pclose | endif
  augroup END
else
	source $HOME/.config/nvim/coc.vim
endif

