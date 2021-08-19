" ========
" Mappings
" ========

" === Leader Mappings ===
" Leader is mapped to ';' in init.vim
" Toggle relative number
nnoremap <Leader>rn :set relativenumber!<CR>
" Open all folds in current buffer (Reduce)
nnoremap <Leader>z zR
" The 3 mappings that I use most often out of all vim mappings :D
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :xa<CR>
nnoremap <Leader>q :qa<CR>
" Clear search
nnoremap <Leader>nh :noh<CR>
" Paste (rarely used because I commonly work in ssh'ed environments)
nnoremap <Leader>pa "+p
" Show what registers contain
nnoremap <Leader>rg :registers<CR>


" === Normal and Universal Mappings ===
" Close a buffer, useful when doing PlugInstall and then closing that
" Or is it close a window? frame? DAMN all this emacs terminology got me so
" confused
nnoremap Q :q<CR>

" NOTE: These mappings Just Work in wsl so no need the extra binding like
" in vimrc, this is why you use neovim instead of vim ;)
noremap <M-j> :m+1<CR>==
noremap <M-k> :m-2<CR>==
noremap <M-J> :t.<CR>==
noremap <M-K> :t.-1<CR>==


" === Visual Mappings ===
" dot command in visual mode
vnoremap . :normal.<CR>

" Move visual selection
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Visual mode pressing * or # searches for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>"

" Keep text selected after indentation
" This is... really useful sometimes but annoying other times
vnoremap < <gv
vnoremap > >gv


" === Window/Buffer/Tab mappings ===
" Better way to move between windows
noremap <C-j> <C-W>j
noremap <C-k> <C-W>k
noremap <C-h> <C-W>h
noremap <C-l> <C-W>l

" Managing buffers
nnoremap <leader>bd :bd<cr>
nnoremap <leader>bn :bnext<cr>
nnoremap <leader>bp :bprev<cr>

" Useful mappings for managing tabs
" Tab create
nnoremap <leader>tc :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
" Tab delete
nnoremap <leader>td :tabclose<cr>
" I rarely have >3 tabs, let alone organize their placements :D but it's here
" because why not
nnoremap <leader>tm :tabmove<cr>
" Switching tabs
nnoremap <leader>tn :tabnext<cr>
nnoremap <leader>tp :tabprev<cr>


" === Misc Mappings ===
" Hooray for neovim :)))))
" Terminal mappings
noremap <C-`> :split term://fish<cr>i
nnoremap <leader>t :split term://fish<cr>i
tnoremap <Esc> <C-\><C-n>

" Command mode mappings
cnoremap <C-p> PlugInstall<cr>
cnoremap <C-f> Format<cr>

" Quickly apply (n)vimrc changes
command! ReloadConfig so $HOME/.config/nvim/init.vim
nnoremap <Leader>rc :ReloadConfig<cr>

