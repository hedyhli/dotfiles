local s = require('nvim-tundra.stylesheet.arctic')

-- See also: statusline.lua for statusline colors (which I also use tundra's
-- stylesheet)

require("nvim-tundra").setup {
  transparent_background = false,
  dim_inactive_windows = { enabled = true, color = nil, },
  sidebars = { enabled = true, color = nil, },
  editor = { search = {}, substitute = {}, },
  syntax = {
    booleans = { bold = true, italic = true },
    comments = { bold = false, italic = false },
    conditionals = {},
    constants = { bold = true },
    fields = {},
    functions = {},
    keywords = {},
    loops = {},
    numbers = { bold = true },
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
      IblIndent = { fg = s.cp.gray._800 },
      IblScope = { fg = s.cp.gray._800 },
      Comment = { fg = s.cp.gray._400 },    -- Add more contrast
      -- Inspired by Modus-Vivendi (emacs).
      -- Shows the matched pair more visibly
      MatchParen = { bg = s.cp.gray._500 },
    },
  },
}

vim.o.background = "dark"
vim.cmd("colorscheme tundra")
