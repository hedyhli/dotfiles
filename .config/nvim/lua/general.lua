local o = vim.opt

o.number = true
o.mouse = "a"

o.cursorline      = true -- highlight current cursor line (this is SO GOOD)
o.showcmd         = true -- show incomplete commands
o.hlsearch        = true -- highlight search
o.wildmenu        = true -- command line's tab complete in a menu
o.errorbells      = false-- no beeps please
o.visualbell      = true -- flash screen instead
o.title           = true -- set window title to file name
o.incsearch       = true -- incrementally find next match while typing search
o.scrolloff = 6     -- screen lines to keep above and below cursor
o.sidescrolloff = 8  -- screen columns to keep on left and right of cursor
o.confirm = true
o.showmatch = true

o.encoding = "utf-8"
o.mat = 2
o.inccommand = "nosplit"  -- neovim only
o.autoindent = true
o.fileformat = "unix"

o.expandtab = true-- AIUI, tab -> spaces
o.softtabstop = 4   -- indent by 2 spaces with tab
o.tabstop = 4       -- show existing tabs with 4 spaces width
o.shiftwidth=4    -- Put or remove 4 spaces with using < and >
o.smarttab = true -- Delete spaces at tabstop width
o.copyindent = true -- Copy indentation from previous line
o.hidden = true   -- Let's you switch to another file while current is unsaved.
o.cmdheight = 2

-- Show LSP W/E hints on another column next to the line numbers
o.signcolumn = "yes"  -- Set to "number" will merge the LSP W/E hints with the number col

-- Bunch of shit really, spent *hours* trying to get tmux + nvim true colors to work
o.termguicolors = true
o.whichwrap:append '<,>,h,l'

-- fold settings
o.foldenable = true
o.foldlevelstart = 10
o.foldnestmax = 10
o.foldmethod = "manual"
o.foldcolumn = "2"

-- Set the swp, backup and undo settings
o.swapfile = false
o.backup = false
o.undodir = vim.fn.expand("~") .. "/.local/share/nvim/undodir/"
o.undofile = true

-- Ignore compiled files
o.wildignore = { "*.o", "*~", "*.pyc", "*/.git/*", "*/.hg/*", "*/.svn/*", "*/.DS_Store" }

-- Return to last edit position when opening files
vim.api.nvim_create_autocmd("BufReadPost", {
	pattern = "*", command = [[if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]]
})

vim.cmd [[ match ErrorMsg '\s\+$' ]]
