-- Note that plugins config may set more mappings.

local silent = { silent = true }
local map = vim.keymap.set
local au = vim.api.nvim_create_autocmd
local function d(s) return { desc = s } end
local function mapbuf(a, b, c) vim.api.nvim_buf_set_keymap(0, a, b, c, { noremap=true }) end

---------------------
-- Leader mappings --
---------------------
map("n", "<Leader>rn", "<cmd>set relativenumber!<cr>", d "toggle rel num")
map("n", "<Leader>z", "zR", d "zA (toggle all folds)")
-- The 3 mappings that I use most often out of all vim mappings :D
map("n", "<Leader>w", "<cmd>w<CR><cmd>echo<CR>", d ":w")
map("n", "<Leader>x", "<cmd>xa<CR>", d ":xa")
map("n", "<Leader>q", "<cmd>qa<CR>", d ":qa")
map("n", "<Leader>nh", "<cmd>noh<CR>", d ":noh")
map("n", "<Leader>p", "\"+p", d "System clipboard paste")
-- Deprecated in favor of telescope registers because you can use <C-e> to edit
-- select register, how cool is that!
-- map("n", "<Leader>rg", "<cmd>registers<CR>", d "Show registers, also <leader>fR")
map("n", "<leader>u", "gul", d "Lower current char")
map("n", "<leader>U", "gUl", d "Upper current char")
map("n", "<leader><up>", ":<up>", d "Like @: but does not <cr>")

-----------------------------------
-- Normal and Universal Mappings --
-----------------------------------
-- Close a buffer, useful when doing PlugInstall and then closing that
-- Or is it close a window? frame? I'll admit all this emacs terminology got me
-- so confused.
map("n", "Q", "<cmd>q<CR>", d":q")

-- Legacy comment incomming
-- > These mappings Just Work in wsl so no need the extra binding like in vimrc,
-- > this is why you use neovim instead of vim ;)
map("n", "<M-j>", "<cmd>m+1<CR>==", d "Move line down")
map("n", "<M-k>", "<cmd>m-2<CR>==", d "Move line up")
map("n", "<M-J>", "<cmd>t.<CR>==", d "Copy line down")
map("n", "<M-K>", "<cmd>t.-1<CR>==", d "Copy line up")

---------------------
-- Visual mappings --
---------------------
-- dot command in visual mode
map("v", ".", "<cmd>normal.<CR>")

-- Move visual selection
-- FIXME: Not working!
map("v", "J", "<cmd>m+1<CR>gv=gv")
map("v", "K", "<cmd>m-2<CR>gv=gv")

-- Visual mode pressing * or # searches for the current selection
map("v", "*", "<cmd><C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>", silent)
map("v", "#", "<cmd><C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>", silent)

-- Keep text selected after indentation
-- This is... really useful sometimes but annoying other times
map("v", "<", "<gv")
map("v", ">", ">gv")

-- Yet another "modern" IDE-like behavior
map("v", "<BS>", "x")


--------------------------------
-- Window/Buffer/Tab mappings --
--------------------------------
-- Better way to move between windows
map("", "<C-j>", "<C-W>j", d "bottom window")
map("", "<C-k>", "<C-W>k", d "top window")
map("", "<C-h>", "<C-W>h", d "right window")
map("", "<C-l>", "<C-W>l", d "left window")

-- Managing buffers
-- map("n", "<leader>bd", "<cmd>bd<cr>")  -- I now use moll/vim-bbye, see plugin_spec.lua
map("n", "<leader>bn", "<cmd>bnext<cr>", d "bnext")
map("n", "<leader>bp", "<cmd>bprev<cr>", d "bprev")

-- Useful mappings for managing tabs
-- Tab create
map("n", "<leader>tc", "<cmd>tabnew<cr>", d "tabnew")
map("n", "<leader>to", "<cmd>tabonly<cr>", d "tabonly")
-- Tab delete
map("n", "<leader>td", "<cmd>tabclose<cr>", d "tabclose")
-- I rarely have >3 tabs, let alone organize their placements <cmd>D but it's here
-- because why not
map("n", "<leader>tm", "<cmd>tabmove<cr>", d "tabmove")
-- Switching tabs
map("n", "<leader>tn", "<cmd>tabnext<cr>", d "tabnext")
map("n", "<leader>tp", "<cmd>tabprev<cr>", d "tabprev")


map("", "<leader>`", "<cmd>botright split term://fish<cr><cmd>resize -7<cr>i")
if vim.fn.has("macunix") == 1 then
  -- WTF?? (I checked using C-v in insert mode)
  -- TODO: Fix in tmux
  map("", "<C-@>", "<cmd>botright split term://fish<cr><cmd>resize -7<cr>i")
else
  map("", "<C-`>", "<cmd>botright split term://fish<cr><cmd>resize -7<cr>i",
    d "Open terminal below" )
end
map("t", "<Esc>", "<C-\\><C-n>")

--------------------------------------
-- Insert/Command Editting Mappings --
--------------------------------------
-- Emacs FTW :)
map({"i", "c"}, "<A-bs>", "<C-w>")
map({"i", "c"}, "<A-h>", "<C-w>")
map({"i", "c"}, "<C-b>", "<left>")
map({"i", "c"}, "<C-f>", "<right>")
map({"i", "c"}, "<A-b>", "<S-left>")
map({"i", "c"}, "<A-f>", "<S-right>")
map("i", "<C-a>", "<C-o>0")
-- map("i", "<C-e>", "<C-o>$")
map("c", "<C-a>", "<C-b>") -- SMH
-- Command mode already supports <C-e>
map("i", "<M-Tab>", "<C-x><C-o>")


--------------------------------
-- Filetype specific mappings --
--------------------------------
au("FileType", {
  pattern = {"help", "qf"},
  callback = function() mapbuf("n", "q", "<cmd>q<cr>") end
})


--------------------------
-- Select mode mappings --
--------------------------
-- Requires 'mini.surround' for 'sa' and Comment.nvim for 'gb'
map("s", '(',  "<C-o>sa(gvll<C-g>", {remap = true})
map("s", ')',  "<C-o>sa)gvll<C-g>", {remap = true})
map("s", '[',  "<C-o>sa[gvll<C-g>", {remap = true})
map("s", ']',  "<C-o>sa]gvll<C-g>", {remap = true})
map("s", '\'', "<C-o>sa'gvll<C-g>", {remap = true})
map("s", '"',  '<C-o>sa"gvll<C-g>', {remap = true})
map("s", '`',  "<C-o>sa`gvll<C-g>", {remap = true})
map("s", '<C-/>',  "<C-v>vgb", {remap = true})

-- Select mode tips (:h Select)
-- * Ctrl-G toggles Visual and Select mode
-- * Ctrl-R <register> let's you put what ever you replaced into register
-- Visual mode mappings apply to select mode unless it is a printable character
-- or <cr>, or above key sequences and <C-o>
--
-- Visual mode tips
-- * You can use 'p' on selection and it will be replaced by paste
-- * 'P' is same except it does not overwrite registers
-- * gn/gN select match
-- * ~ Switch case (or u/U)
-- Visual block
-- * I/A can be used in place of i/a
-- * >/< shift can be used!

vim.api.nvim_create_user_command('Date', [[exe "r!date '+\\%Y-\\%m-\\%d \\%T'" | normal! kJ]], {})
