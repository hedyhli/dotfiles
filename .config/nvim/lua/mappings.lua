-- Note that plugins config may set more mappings.

local silent = { silent = true }
local function d(s) return { desc = s } end
local function map(...) vim.keymap.set(...) end
-- ==============-=
-- Leader mappings
-- ===============
map("n", "<Leader>rn", "<cmd>set relativenumber!<cr>", d "toggle rel num")
-- Open all folds in current buffer (Reduce)
map("n", "<Leader>z", "zR", d "zR (open all folds)")
-- The 3 mappings that I use most often out of all vim mappings :D
map("n", "<Leader>w", "<cmd>w<CR>", d "write")
map("n", "<Leader>x", "<cmd>xa<CR>", d "x all")
map("n", "<Leader>q", "<cmd>qa<CR>", d "quit all")
-- Clear search
map("n", "<Leader>nh", "<cmd>noh<CR>", d ":noh")
-- Paste (rarely used because I commonly work in ssh'ed environments)
map("n", "<Leader>pa", "+p", d "System clipboard paste")
-- Show what registers contain
map("n", "<Leader>rg", "<cmd>registers<CR>", d "Show registers")


-- =============================
-- Normal and Universal Mappings
-- =============================
-- Close a buffer, useful when doing PlugInstall and then closing that
-- Or is it close a window? frame? DAMN all this emacs terminology got me so
-- confused
map("n", "Q", "<cmd>q<CR>")

-- NOTE: These mappings Just Work in wsl so no need the extra binding like
-- in vimrc, this is why you use neovim instead of vim ;)
map("", "<M-j>", "<cmd>m+1<CR>==")
map("", "<M-k>", "<cmd>m-2<CR>==")
map("", "<M-J>", "<cmd>t.<CR>==")
map("", "<M-K>", "<cmd>t.-1<CR>==")

-- ===============
-- Visual mappings
-- ===============
-- dot command in visual mode
map("v", ".", "<cmd>normal.<CR>")

-- Move visual selection
map("v", "J", "<cmd>m '>+1<CR>gv=gv")
map("v", "K", "<cmd>m '<-2<CR>gv=gv")

-- Visual mode pressing * or # searches for the current selection
map("v", "*", "<cmd><C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>", silent)
map("v", "#", "<cmd><C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>", silent)

-- Keep text selected after indentation
-- This is... really useful sometimes but annoying other times
map("v", "<", "<gv")
map("v", ">", ">gv")

-- Yet another "modern" IDE-like behavior
map("v", "<BS>", "x")


-- ==========================
-- Window/Buffer/Tab mappings
-- ==========================
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


if vim.fn.has("macunix") == 1 then
  -- WTF?? (I checked using C-v in insert mode)
  -- TODO: Fix in tmux
  map("", "<C-@>", "<cmd>split term://fish<cr><cmd>resize -7<cr>i")
else
  map("", "<C-`>", "<cmd>split term://fish<cr><cmd>resize -7<cr>i",
    d "Open terminal below" )
end
-- <leader>t prefix is used for tab navigation mappings
-- map("n", "<leader>t", "<cmd>echom 'Deprecated. Please use C-` instead'<cr>"
--   {desc = "Please use <C-`>"})
map("t", "<Esc>", "<C-\\><C-n>", d "Esc in terminal mode")
