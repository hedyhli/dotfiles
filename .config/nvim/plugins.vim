" ==========================
" Plugins and their settings
" ==========================
"
" === Plugin declarations ===
call plug#begin(stdpath('data') . '/plugged')

" dracula color theme (THE most important plugin, yes)
Plug 'dracula/vim', {'name': 'dracula'}
Plug 'preservim/nerdtree'          " Perhaps switch to nvim-tree when I drop
                                   " v0.5 support
Plug 'Xuyuanp/nerdtree-git-plugin' " Show git statuses in NERDTree
Plug 'Yggdroot/indentLine'         " Show indentation levels with vertical bars
Plug 'tpope/vim-fugitive'          " git commands
Plug 'jreybert/vimagit'            " emacs' magit ✨
Plug 'tpope/vim-surround'          " quoting and parenthesizing manipulation
Plug 'tpope/vim-commentary'        " I'd rather not clog <Leader> mappings
                                   " with nerd commentor
Plug 'itchyny/lightline.vim' " airline was throwing shitty errors so yeah.
                             " Pretty and customizable status bar; faster than
                             " other vim statusline plugins, for components
                             " see below; TODO: switch to galaxyline, lualine
                             " or express_line in the future?
" Commented out because I realized I never use it (lol)
" Plug 'mbbill/undotree'  " undo tree
Plug 'bling/vim-bufferline'   " buffer line (one of my most used plugins!)
Plug 'ctrlpvim/ctrlp.vim'     " Quickly find a fine with fuzzy find
                              " TODO: use telescope instead
Plug 'airblade/vim-gitgutter' " Show git diff overview stuff in the left
                              " column
Plug 'majutsushi/tagbar'      " Quickly jump to a symbol in buffer (one of my
                              " most used). Requires exuberant ctags
Plug 'tpope/vim-endwise'      " Add those 'endif'/'fi'/'done'
                              " TODO: work with nvim-cmp

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

" === LSP and autopair plugins ===
" This plugin below is really good, but whenever I'm on a commented line,
" press o, press backspace, the line below is joined up above. You won't
" believe how long it took me to debug this problem and finally realize it's
" because of this plugin.
" Plug 'jiangmiao/auto-pairs'
"
" So now I'm using these instead:
" 1. windwp/nvim-autopairs - extremely customizable, written in lua -
"    integrates wth hrsh7th/nvim-cmp
" 2. townk/vim-autoclose   - fallback if nvim < v0.5
if has('nvim-0.5')
  if has('nvim-0.7')
    Plug 'windwp/nvim-autopairs'
  else
    Plug 'windwp/nvim-autopairs', { 'commit': 'b7672cd' }
  endif
  " Best IDE autocomplete setup ever
  Plug 'neovim/nvim-lspconfig'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'    " Completions of words in current buffer
  Plug 'hrsh7th/cmp-path'      " File paths
  Plug 'hrsh7th/cmp-cmdline'   " Fire your way through the neovim cmd line
  Plug 'hrsh7th/cmp-calc'
  Plug 'hrsh7th/cmp-emoji'     " 😏 :smirk:
  Plug 'kdheepak/cmp-latex-symbols'  " τ long live \tau
  Plug 'hrsh7th/nvim-cmp'
  " Snippets
  if has('nvim-0.7')
    Plug 'dcampos/nvim-snippy'
    Plug 'dcampos/cmp-snippy'
  endif
else
  Plug 'townk/vim-autoclose'
  " Picking the right LSP completion method, see bottom of file for more
  if has('node')
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
  endif
endif

call plug#end()
" Plugin declarations ends here


colorscheme dracula  " THE most important nvim configuration


" === Statusline ✨ ===
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
      \   'morelineinfo': 'LightlineMoreLineinfo',
      \ },
      \ 'component_type': {
      \   'diagnosticscount': 'error',
      \ },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['tagbar'] ],
      \		'right': [ [ 'diagnosticscount' ], ['morelineinfo'], ['fileformat', 'fileencoding', 'filetype'] ]
      \ },
      \ }

function! LightlineMoreLineinfo()
  " Like lineinfo but appends total line number:
  " 123:4/200
  " cur line : cur col / total lines
  return printf("%10s", line('.').':'.col('.').'/'.line('$'))
endfunction

function! LightlineFugitive()
  " Referenced from Lightline docs; I'm not 100% sure what this does but seems
  " like it just grabs the current git branch
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

function! GetDiagnosticCount(level)
  return len(luaeval('vim.diagnostic.get(0, {severity = vim.diagnostic.severity.' . a:level . '})'))
endfunction

function! LightlineDiagnostics()
  " Grabs the current buffer's diagnostics count and displays it in the
  " format 'E:x W:x I:x H:x'
  " If all 4 types of diagnostics are 0 then don't display anything
  if !has('nvim-0.5')
    " TODO: if we don't have nvim-0.5 then call some CoC function?
    return '!0.5'
  endif

  let errors = GetDiagnosticCount('ERROR')
  let warnings = GetDiagnosticCount('WARN')
  let infos = GetDiagnosticCount('INFO')
  let hints = GetDiagnosticCount('HINT')
  if (errors + warnings + infos + hints) == 0
    return ''
  endif

  let string = ''
  if errors != 0
    let string = string . 'E:' . errors . ' '
  endif
  if warnings != 0
    let string = string . 'W:' . warnings . ' '
  endif
  if infos != 0
    let string = string . 'I:' . infos . ' '
  endif
  if hints != 0
    let string = string . 'H:' . hints . ' '
  endif

  return substitute(string, '\s$', '', '')
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
      \ "Unknown"   : "?"
      \ }

" tagbar
nnoremap <leader>tt :TagbarToggle<CR>
let g:tagbar_width = 20

" Make indentLine still work, but disable conceal characters. eg, hiding the
" asterisks for a bold text in markdown is extremely annoying when you want to
" delete or change those asterisks.
let g:markdown_syntax_conceal=0
let g:vim_json_conceal=0

" === LSP ===
if has("nvim-0.5")
  " Set completeopt to have a better completion experience
  set completeopt=menuone,noinsert,noselect,preview
  lua require('autopair')
  lua require('lsp')
  lua require('complete')
else
  if has('node')
    source $HOME/.config/nvim/coc.vim
  endif
endif

