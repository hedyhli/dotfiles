-- https://github.com/simrat39/symbols-outline.nvim
-- Switched to this from tagbar because it does not require exctags, and it
-- lists the items in order as defined in source code.
-- Then I ended up forking it...
-- https://github.com/hedyhli/outline.nvim

return {
  {
  dir = "~/projects/outline.nvim",
  enabled = vim.fn.has("nvim-0.7") == 1,
  lazy = false,
  cmd = { "Outline", "OutlineOpen" },
  keys = {
    { "<leader>tt", "<cmd>Outline<CR>", desc = "Toggle outline window" },
    { "<leader>t<leader>", "<cmd>Outline!<CR>", desc = "Toggle outline window without focus" },
    { "<leader>tf", "<cmd>OutlineFollow<CR>", desc = "Focus & follow outline window" },
  },
  opts = {
    preview_window = {
      border = 'rounded',
      open_hover_on_preview = false,
    },
    symbol_folding = {
      -- Auto fold all but current hover
      autofold_depth = 1,
      auto_unfold_nodes = {
        hovered = true,
        only = 2,
      },
    },
    guides = {
      -- Keep only guides that indicate siblings that might span multiple lines (vertical)
      markers = {
        middle = ' ',
        bottom = ' ',
      }
    },
    keymaps = {
      close = 'q',
      unfold_all = {},
      fold_all = {},
      fold_toggle = {'<tab>', '<space>'},
    },
    outline_window = {
      position = 'left',
      show_cursorline = 'focus_in_outline',
      -- Beautiful, but not very good in indicating window focus (I don't use a
      -- statusline), plus at the time of writing preview window breaks this
      -- feature... But still beautiful nonetheless!
      hide_cursor = true,
      winhl = "OutlineDetails:LineNr,OutlineLineno:LineNr,OutlineGuides:Comment",
    },
    outline_items = {
      auto_set_cursor = false,
    },
    symbols = {
      filter = {
        lua = { 'String', 'Package', 'Constant', exclude = true },
      },
      icon_fetcher = function(k)
        local buf = vim.api.nvim_win_get_buf(require('outline').state.code_win)
        local ft = vim.api.nvim_buf_get_option(buf, "ft")
        -- There can only be kind String in markdown so... let's not have the
        -- eye candy here
        if ft == 'markdown' and k == 'String' then
          return ""
        end
        return false
      end,
      icon_source = "lspkind",
    },
  },
},
  -- For testing when users from this repo reports issues.
  { "simrat39/symbols-outline.nvim", config=true },
}
