local function config()

local cp = require('nvim-tundra.palette.arctic')
local a = vim.api
local f = vim.fn

local mode_colors = {
  n = "=Comment",
  v = cp.green._500,
  V = cp.green._500,
  ["CTRL-V"] = cp.green._500,
  R = cp.red._500,
  i = cp.sand._500,
  s = cp.orange._500,
  S = cp.orange._500,
  ["CTRL-S"] = cp.orange._500,
}

local function get_mode_fghl(mode)
  local hl = "#Normal"
  if mode_colors[mode.prefix] then
    hl = mode_colors[mode.prefix]
  end
  if mode_colors[mode.mode] then
    hl = mode_colors[mode.mode]
  end
  if mode.blocking then
    hl = "=Error"
  end
  return hl
end

local function get_win_maxy()
  local max_y_pos = 0
  local max_y_win = 0
  for _, winid in ipairs(a.nvim_list_wins()) do
    local c = a.nvim_win_get_config(winid)
    -- Loop only proper windows (not floating etc)
    if c.relative == "" then
      if max_y_pos <= a.nvim_win_get_position(winid)[1] then
        max_y_win = winid
      end
    end
  end
  return max_y_win
end

function _G.Incrender(p)
  -- p.buf, win, focused
  -- Hide incline for bottomost win, because I have ruler
  if get_win_maxy() == p.win then
    return ""
  end

  local fp = a.nvim_buf_get_name(p.buf)
  fp = f.fnamemodify(fp, ":t") or '[No Name]'
  local mod = ''
  if a.nvim_buf_get_option(p.buf, 'modified') then
    mod = '+'
  end

  local mode = a.nvim_get_mode()
  mode.prefix = mode.mode:sub(1,1)
  mode.fghl = get_mode_fghl(mode)
  local r_mode = { mode.mode, guifg = mode.fghl }
  if mode.fghl:sub(1, 1) == '=' then
    -- group takes higher precedence than any other hl options
    r_mode.group = mode.fghl:sub(2, #mode.fghl)
  end

  local r = { group = "Comment", r_mode, ' ', fp, mod }
  if not p.focused then
    r.group = "LineNr"
    r_mode.group = "LineNr"
  end
  return r
end
-- local render = function(props)
--   local bufname = a.nvim_buf_get_name(props.buf)
--   local res = bufname ~= '' and f.fnamemodify(bufname, ':t') or '[No Name]'
--   if a.nvim_buf_get_option(props.buf, 'modified') then
--     res = res .. ' +'
--   end
--   return res
-- end

require('incline').setup {
  render = _G.Incrender,
  debounce_threshold = {
    falling = 50,
    rising = 10
  },
  hide = {
    cursorline = false,
    focused_win = false,
    only_win = true,
  },
  highlight = {
    groups = {
      InclineNormal = {
        default = true,
        group = "NormalFloat"
      },
      InclineNormalNC = {
        default = true,
        group = "NormalFloat"
      }
    }
  },
  ignore = {
    buftypes = {},
    filetypes = {},
    floating_wins = true,
    unlisted_buffers = true,
    wintypes = {},
  },
  window = {
    margin = {
      horizontal = 0,
      -- Needs = 1 if setting &lines+=1
      vertical = { top = 0, bottom = 0 }
    },
    options = {
      signcolumn = "no",
      wrap = false
    },
    padding = 0,
    padding_char = " ",
    placement = {
      horizontal = "right",
      vertical = "bottom"
    },
    width = "fit",
    winhighlight = {
      active = {
        EndOfBuffer = "None",
        Normal = "InclineNormal",
        Search = "None"
      },
      inactive = {
        EndOfBuffer = "None",
        Normal = "InclineNormalNC",
        Search = "None"
      }
    },
    zindex = 50
  }
}
end

return {
  "b0o/incline.nvim",
  config = config,
}
