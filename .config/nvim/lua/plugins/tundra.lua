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
      TreesitterContext = { bg = s.cp.gray._700 },
      -- FIXME: Line number hl doesn't work
      TreesitterContextLineNumber = { fg = s.cp.gray._500, bg = s.cp.gray._700 },
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
