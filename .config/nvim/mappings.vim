"""""""""""
" Mappings
""""""""""
" leader mappings
nnoremap <Leader>rn :set relativenumber!<CR>
nnoremap <Leader>z zR
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :xa<CR>
nnoremap <Leader>q :qa<CR>

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

noremap <M-j> :m+1<CR>==
noremap <M-k> :m-2<CR>==

noremap <M-J> :t.<CR>==
noremap <M-K> :t.-1<CR>==

" Visual mode pressing * or # searches for the current selection
"  Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>"

" Smart way to move between windows
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l

" managing buffers
nnoremap <leader>bd :bd<cr>
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :b<cr>

" Useful mappings for managing tabs
noremap <leader>tn :tabnew<cr>
noremap <leader>to :tabonly<cr>
noremap <leader>tc :tabclose<cr>
noremap <leader>tm :tabmove
noremap <leader>t<leader> :tabnext
noremap <leader>bn :bnext<cr>
noremap <leader>bp :bprev<cr>

" terminal mappings
noremap <C-`> :split term://fish<cr>i
noremap <leader>t :split term://fish<cr>i
tnoremap <Esc> <C-\><C-n>

" command mode mappings
cnoremap <C-p> PlugInstall<cr>
cnoremap <C-f> Format<cr>

" my commands
command! ReloadConfig so $HOME/.config/nvim/init.vim
nnoremap <Leader>rc :ReloadConfig<cr>

" keep text selected after indentation
vnoremap < <gv
vnoremap > >gv
