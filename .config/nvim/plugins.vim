call plug#begin(stdpath('data') . '/plugged')

Plug 'dracula/vim', {'name': 'dracula'} " dracula color theme
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'prettier/vim-prettier', { 'do': 'npm install'  }
Plug 'Yggdroot/indentLine'  " indentLine plugin
Plug 'stautob/vim-fish'  " fish support for vim
Plug 'tpope/vim-fugitive'  " git stuff
Plug 'tpope/vim-surround'  " quoting and parenthesizing plugin
Plug 'jiangmiao/auto-pairs'  " quote pairs and other neat stuff
Plug 'https://github.com/tpope/vim-commentary'
Plug 'vim-airline/vim-airline'  " airline plugin for status bar
Plug 'mbbill/undotree'  " undo tree
Plug 'bling/vim-bufferline'  " buffer line
Plug 'tpope/vim-fugitive'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'airblade/vim-gitgutter'
Plug 'majutsushi/tagbar'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'adelarsq/vim-hackernews'
if has('python')
	Plug 'laurentgoudet/vim-howdoi'
endif
Plug 'wakatime/vim-wakatime'  " wakatime for vim

call plug#end()

"let g:dracula_colorterm = 0
colorscheme dracula

" airline integrations
let g:airline#extensions#tagbar#enabled = 1

" open current dir in nerdtree
noremap <Leader>nf :NERDTreeFind<CR>
" open nerdtree when openning a dir in vim
let NERDTreeWinSize = 20
autocmd vimenter * NERDTree % | exe "normal \<c-w>l"
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

" coc-yank
nnoremap <silent> <space>y  :<C-u>CocList -A --normal yank<cr>


" coc settings
" ==== coc installs ====
" coc-go
" coc-html
" coc-snippets
" coc-python
" coc-marketplace

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <F2> <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)


" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

