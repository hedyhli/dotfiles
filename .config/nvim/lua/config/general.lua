local o = vim.opt

-- can be toggled with <leader>rn (see mappings.lua).
-- showing '0' in current row in vim is the most useless thing ever in all
-- software known to humanity.
o.number = true
o.relativenumber = true

o.mouse = "a"
-- Use select mode when dragging with mouse.
-- This allows IDE-like quick substitution. If I need to use the mouse for
-- visual selection, I can select with mouse, then <C-v>v
-- See mappings.lua for IDE-like pair wrapping in select mode. (Select text,
-- press an open bracket to have selection be wrapped immediately.)
o.selectmode = "mouse"

-- Changes to default:
-- - Use vertical in command mode
o.guicursor = "n-v-sm:block,i-ci-ve-c:ver25,r-cr-o:hor20"

-- o.cursorline      = true
-- Cursor line is rather distracting when trying to focus on the code.
-- Especially so when typing in comments -- the background of the cursor line
-- behind the already low-constrast comment fg color makes it hard to read (and
-- write with) <-- this was back when using dracula.
--
-- If I'm working on something with long lines and I need a visual indicator of
-- alignment I can do visual line then <Esc> to "blink" the cursor line, or
-- enable this locally.
--
-- Maybe I'll figure out a way in the future to dynamically enable cursorline
-- when screenline is wrapped.
o.showcmd         = true -- show incomplete commands
o.hlsearch        = true -- highlight search
o.wildmenu        = true -- command line's tab complete in a menu
o.errorbells      = false-- no beeps please
o.visualbell      = true -- flash screen instead
o.title           = true -- set window title to file name
o.incsearch       = true -- incrementally find next match while typing search
-- Having scrolloff positive means I can't have cursor at the very top/bottom
-- whilst referring to some content on the other side of the same buffer
o.scrolloff       = 0
o.sidescrolloff   = 0
o.confirm         = true
o.showmatch       = true
o.showmode        = false -- Don't show messages when switching modes
                          -- It's such an integral part of (n)vim, I know what I'm doing :)

o.encoding = "utf-8"
o.matchtime = 1           -- deci second to show matching pair while typing
o.inccommand = "nosplit"  -- neovim only
o.autoindent = true
o.fileformat = "unix"

o.splitbelow = true
o.splitright = true
if vim.fn.has("nvim-0.9") == 1 then
  -- Could be potentially dangerous before the exrc PR is merged.
  -- In 0.9 they can open files securely using trust DB
  o.exrc = true
end
vim.o.autochdir = true

o.expandtab     = true -- AIUI, tab -> spaces
o.softtabstop   = 4    -- indent by 4 spaces with tab
o.tabstop       = 4    -- show existing tabs with 4 spaces width
o.shiftwidth    = 4    -- Put or remove 4 spaces with using < and >
o.smarttab      = true -- Delete spaces at tabstop width
o.copyindent    = true -- Copy indentation from previous line
o.hidden        = true -- Allows switching to another file while current is unsaved.

o.showcmdloc = "last"

-- lualine, feline, galaxyline? RULER + inc-line!
-- Used when there is only one horizontal split
o.rulerformat   = "%#LineNr#%20(%f %p%%%)"
-- Needed to show "recording @" and other messages until this issue is resolved
-- in neovim core
o.cmdheight     = 1
o.laststatus    = 0
vim.api.nvim_set_hl(0, 'Statusline', {link = 'Normal'})
vim.api.nvim_set_hl(0, 'StatuslineNC', {link = 'Normal'})
o.statusline = '%#WinSeparator#'.. ("─"):rep(vim.api.nvim_win_get_width(0)) .. '%*'
-- Clear cmdline
vim.keymap.set("n", "<leader><leader>", "<cmd>echo<cr>")

-- Show LSP W/E hints on another column next to the line numbers
o.signcolumn = "yes"  -- Set to "number" will merge the LSP W/E hints with the number col

-- Bunch of shit really, spent *hours* trying to get tmux + nvim true colors to work
-- Dracula theme without termguicolors looks like poo; see my vimrc.
o.termguicolors = true

o.whichwrap:append '<,>,h,l'

o.foldenable = true
o.foldlevelstart = 10
o.foldnestmax = 10
-- o.foldcolumn = "2"
o.foldmethod = "expr"
o.foldexpr = "nvim_treesitter#foldexpr()"

o.swapfile = false
-- This is used to control how often to update swap file. Since I don't
-- use swap, I can control CursorHold time with this.
o.updatetime = 3000
o.backup = false
o.undodir = vim.fn.expand("~") .. "/.local/share/nvim/undodir/"
o.undofile = true

-- Ignore compiled files
o.wildignore = { "*.o", "*~", "*.pyc", "*/.git/*", "*/.hg/*", "*/.svn/*", "*/.DS_Store" }

-- Not too noisy visually, but visible enough as a reminder.
-- Underlines trailing spaces.
-- vim.cmd [[ match Underlined '\s\+$' ]]
--
-- mini.trailspace FTW
