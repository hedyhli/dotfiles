-- NOTE that I no longer use lualine (or any statusline at all)
--- Helpers for status line components ---
-- local function lineinfo()
--   local total = tostring(vim.fn.line('$'))
--   -- Left pad the current line count to max width of this value, to avoid jittering
--   return string.format("%"..total:len().."d", vim.fn.line('.'))..'/'..total
-- end

local function mode_fmt(str)
  if str == "NORMAL" then
    return "./"
  elseif str == "INSERT" then
    return "i "
  elseif str == "COMMAND" then
    return ":>"
  elseif str == "VISUAL" then
    return "v "
  elseif str == "V-BLOCK" then
    return "^v"
  elseif str == "V-LINE" then
    return "VL"
  elseif str == "TERMINAL" then
    return ":D"
  elseif str == "REPLACE" then
    return "R "
  elseif str == "SUBSTITUTE" then
    return "SB"
  elseif str == "SELECT" then
    return "SL"
  end
  return str
end

local diagnostics_config = {
  'diagnostics',
  symbols = {error = '', warn = '', info = 'I:', hint = 'H:'},
}

local competitest_line = {
  filetypes = {'CompetiTest'},
  inactive_sections = {
    lualine_c = { function()
                    return vim.b.competitest_title or 'CompetiTest'
                  end },
  },
}

--- Theme ---
-- https://github.com/sam4llis/nvim-tundra/blob/main/lua/lualine/themes/tundra.lua
local cp = require('nvim-tundra.palette.arctic')
local tundra_lualine = {
  normal = {
    a = { fg = cp.gray._300, bg = cp.gray._700, gui = 'bold' },
    b = { fg = cp.gray._300, bg = cp.gray._800, gui = 'bold' },
    c = { fg = cp.gray._300, bg = cp.gray._800 },
  },

  insert = {
    a = { fg = cp.sand._500, bg = cp.gray._700, gui = 'bold' },
    b = { fg = cp.gray._700, bg = cp.gray._700, gui = 'bold' },
    c = { fg = cp.gray._300, bg = cp.gray._800 },
  },

  visual = {
    a = { fg = cp.gray._300, bg = cp.green._900, gui = 'bold' },
    b = { fg = cp.green._900, bg = cp.transparent, gui = 'bold' },
  },

  replace = {
    a = { fg = cp.gray._300, bg = cp.red._800, gui = 'bold' },
    b = { fg = cp.red._800, bg = cp.transparent, gui = 'bold' },
  },

  command = {
    a = { fg = cp.indigo._500, bg = cp.gray._600, gui = 'bold' },
    b = { fg = cp.gray._600, bg = cp.transparent, gui = 'bold' },
  },

  -- visual_select = {
  --   a = { fg = cp.gray._200, bg = cp.sand._500, gui = 'bold' },
  --   b = { fg = cp.gray._600, bg = cp.transparent, gui = 'bold' },
  -- },
}

local lualine = require('lualine')
lualine.setup {
  options = {
    icons_enabled = true,
    -- theme = 'auto',
    theme = tundra_lualine,
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {'CompetiTest'},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
  },
  inactive_sections = {
    -- lualine_a = {},
    -- lualine_b = {'branch', 'diff'},
    -- lualine_c = {'filename'},
    -- lualine_x = { diagnostics_config },
    -- lualine_y = {'searchcount'},
    -- lualine_z = {},
  },
  tabline = {
    lualine_a = {{ 'mode', fmt = mode_fmt }},
    lualine_b = {},
    lualine_c = {
      { 'buffers',
        mode = 4,
        hide_filename_extension = true,
        buffers_color = {
          -- Same values as the general color option can be used here.
          active = 'lualine_z_active',
          inactive = 'lualine_c',
        },
        symbols = {
          modified = '+',      -- Text to show when the buffer is modified
          alternate_file = '', -- Text to show to identify the alternate file
          directory =  '',     -- Text to show when the buffer is a directory
        },
      }
    },
    lualine_x = {'diff', 'encoding', { 'fileformat',
        icons_enabled = true,
        -- symbols = { unix = 'LF', dos = 'CRLF', mac = 'CR', },
      }, 'filetype'},
    lualine_y = {'searchcount'},
    lualine_z = {diagnostics_config},
  },
  winbar = {},
  inactive_winbar = {},
  extensions = {'nvim-tree', competitest_line, 'symbols-outline'},
}
