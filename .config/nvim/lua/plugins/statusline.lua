local function lineinfo()
  local total = tostring(vim.fn.line('$'))
  -- Left pad the current line count to max width of this value, to avoid jittering
  return string.format("%"..total:len().."d", vim.fn.line('.'))..'/'..total
end

local diagnostics_config = {
  'diagnostics',
  symbols = {error = 'E:', warn = 'W:', info = 'I:', hint = 'H:'},
}

require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = { left = '/', right = '/'},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {{ 'mode', fmt = function(str)
      if str == "NORMAL" then
        return "NORM"
      elseif str == "INSERT" then
        return "INS"
      elseif str == "COMMAND" then
        return "CMD"
      elseif str == "VISUAL" then
        return "VIS"
      elseif str == "REPLACE" then
        return "REP"
      elseif str == "VISUAL BLOCK" then
        return "VBL"
      elseif str == "TERMINAL" then
        return "TERM"
      elseif str == "SUBSTITUTE" then
        return "SUB"
      end
      return str
    end }},
    lualine_b = {'branch', 'diff'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', { 'fileformat',
        icons_enabled = true,
        symbols = { unix = 'LF', dos = 'CRLF', mac = 'CR', },
      }, 'filetype'},
    lualine_y = {diagnostics_config},
    lualine_z = {lineinfo},
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {'brach', 'diff'},
    lualine_c = {'filename'},
    lualine_x = {diagnostics_config},
    lualine_y = {'searchcount'},
    lualine_z = {lineinfo},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {'nvim-tree'},
}

