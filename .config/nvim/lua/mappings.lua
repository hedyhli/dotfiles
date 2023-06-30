local silent = { silent = true }
local function map(...) vim.keymap.set(...) end
-- ==============-=
-- Leader mappings
-- ===============
map("n", "<Leader>rn", '<cmd>set relativenumber!<cr>')
-- Open all folds in current buffer (Reduce)
map("n", "<Leader>z", "zR")
-- The 3 mappings that I use most often out of all vim mappings :D
map("n", "<Leader>w", "<cmd>w<CR>")
map("n", "<Leader>x", "<cmd>xa<CR>")
map("n", "<Leader>q", "<cmd>qa<CR>")
-- Clear search
map("n", "<Leader>nh", "<cmd>noh<CR>")
-- Paste (rarely used because I commonly work in ssh'ed environments)
map("n", "<Leader>pa", "+p")
-- Show what registers contain
map("n", "<Leader>rg", "<cmd>registers<CR>")


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
-- VIsual mappings
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


-- ==========================
-- Window/Buffer/Tab mappings
-- ==========================
-- Better way to move between windows
map("", "<C-j>", "<C-W>j")
map("", "<C-k>", "<C-W>k")
map("", "<C-h>", "<C-W>h")
map("", "<C-l>", "<C-W>l")

-- Managing buffers
-- map("n", "<leader>bd", "<cmd>bd<cr>")  -- I now use moll/vim-bbye, see plugin_spec.lua
map("n", "<leader>bn", "<cmd>bnext<cr>")
map("n", "<leader>bp", "<cmd>bprev<cr>")

-- Useful mappings for managing tabs
-- Tab create
map("n", "<leader>tc", "<cmd>tabnew<cr>")
map("n", "<leader>to", "<cmd>tabonly<cr>")
-- Tab delete
map("n", "<leader>td", "<cmd>tabclose<cr>")
-- I rarely have >3 tabs, let alone organize their placements <cmd>D but it's here
-- because why not
map("n", "<leader>tm", "<cmd>tabmove<cr>")
-- Switching tabs
map("n", "<leader>tn", "<cmd>tabnext<cr>")
map("n", "<leader>tp", "<cmd>tabprev<cr>")


if vim.fn.has("macunix") == 1 then
  -- WTF?? (I checked using C-v in insert mode)
  -- TODO: Fix in tmux
  map("", "<C-@>", "<cmd>split term://fish<cr>i")
else
  map("", "<C-`>", "<cmd>split term://fish<cr>i")
end
map("n", "<leader>t", "<cmd>echom 'Deprecated. Please use C-` instead'<cr>")
map("t", "<Esc>", "<C-\\><C-n>")

-- SHUSH
map("n", "<Leader>lsh", "<cmd>LspStop")
map("n", "<Leader>lst", "<cmd>LspStart")


vim.api.nvim_create_autocmd("BufReadPost", {
    pattern = "*",
    callback = function()
      vim.b.lsp_lines_enabled = false
    end
})

map("n", "<Leader>lx", function()
    require("lsp_lines").toggle()
    -- Disable virtual_text since it's redundant due to lsp_lines.
    if vim.b.lsp_lines_enabled then
      -- IT was enabled, now it's disabled.
      vim.diagnostic.config({ virtual_text = true })
      vim.b.lsp_lines_enabled = false
    else
      vim.diagnostic.config({ virtual_text = false })
      vim.b.lsp_lines_enabled = true
    end
  end
)
