-- { "dracula/vim",
--   name = "dracula", lazy = false, priority = 10000,
--   config = function() vim.cmd("colorscheme dracula") end,
--   enabled = false, -- LOL I've switched to tundra semi-temporarily
-- },
-- The thing about using dracula theme in nvim here is the annoyance of having
-- to use lua APIs to interact with something that was designed for vimscript.
-- With "modern" nvim plugins where the themes are written in 100% lua,
-- configuring everything else (such as statusline colors) reusing values
-- defined in these themes provide a much smoother, cleaner configuration.
--
-- This is by no means a /political/ (ahem) statement suggesting my abandoning
-- of the holy dracula theme. I just, well, decided to try something new since
-- I sort of realized there is no "perfect" or "best" theme.
--
-- After hours of blood, pain, and tears of trying to tweak dracula colors to
-- be easier on the eyes and have greater contrast, I took one look at the
-- carousel of themes from NvChad and fell in love with tundra...
--
-- So I did find dracula.nvim, but I've already integrated tundra well.

local function config()
local s = require('nvim-tundra.stylesheet.arctic')

-- See also: statusline.lua for statusline colors (which I also use tundra's
-- stylesheet)

-- bg.cursorline is used elsewhere, rather than digging into all the places
-- that they are used and make it lighter (increase contrast) I'd change change
-- the field here, and optionally set the actual Cursorline highlight group
-- back if needed.
s.bg.cursorline = s.cp.gray._700

require("nvim-tundra").setup {
  transparent_background = false,
  dim_inactive_windows = { enabled = true },
  sidebars = { enabled = true },
  editor = { search = {}, substitute = {}, },
  syntax = {
    booleans = {},
    comments = {},
    conditionals = {},
    constants = { bold = true },
    fields = {},
    functions = {},
    keywords = {},
    loops = {},
    numbers = {},
    operators = { bold = true },
    punctuation = {},
    strings = {},
    types = { italic = true },
  },
  diagnostics = { errors = {}, warnings = {}, information = {}, hints = {}, },
  plugins = {
    lsp = true,
    semantic_tokens = true,
    treesitter = true,
    telescope = true,
    nvimtree = true,
    cmp = true,
    context = true,
    dbui = true,
    gitsigns = true,
    neogit = true,
    textfsm = true,
  },
  overwrite = {
    colors = {},
    highlights = {
      TreesitterContext = { bg = "#283343" },
      Folded = {
        fg = s.cp.gray._400,
        bg = s.cp.gray._700
      },
      IblIndent = { fg = s.cp.gray._1000 },
      IblScope = { fg = s.cp.opal._500 },
      Comment = { fg = s.cp.gray._400 },    -- Add more contrast
      -- Inspired by Modus-Vivendi (emacs).
      -- Shows the matched pair more visibly
      MatchParen = { bg = s.cp.gray._500 },
      -- Fix completion menu border color
      FloatBorder = { fg = s.cp.gray._500 },
    },
  },
}

vim.o.background = "dark"
vim.cmd("colorscheme tundra")
end

return {
  -- Must be loaded before ibl!
  "sam4llis/nvim-tundra", lazy = false, priority = 10000,
  config = config,
}

-- return {
--   "craftzdog/solarized-osaka.nvim",
--   lazy = false,
--   priority = 1000,
--   config = function()
--     require('solarized-osaka').setup({
--       transparent = false,
--       styles = {
--         floats = "transparent",
--       }
--     })
--     vim.cmd("colorscheme solarized-osaka")
--   end
-- }
