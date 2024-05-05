-- Ordered in discovery date of these high-quality, self-contained and
-- minimalist plugins. Finding myself using switching to more of more of
-- ones from the library and eventually found the need to extract it to a
-- dedicated file.

return {
  { "echasnovski/mini.files",
    -- Neat popout window to browse dirs and manipulate within the editor!
    cmd = "MiniFiles",
    keys = { { "<leader>E", "<cmd>MiniFiles<cr>",  desc = ":MiniFiles" } },
    -- Mini.files stopped working a while ago, for some reason. I've switched
    -- to oil.nvim.
    enabled = false,
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require('mini.files').setup{}
      vim.api.nvim_create_user_command('MiniFiles', function()
        require('mini.files').open(nil, false)
      end, { desc = ":lua MiniFiles.open()" })
    end,
  },
  { "echasnovski/mini.surround", version = '*',
    opts = {
      -- Duration (in ms) of highlight when calling `MiniSurround.highlight()`
      highlight_duration = 500,
      -- Module mappings. Use `''` (empty string) to disable one.
      mappings = {
        add = 'sa',
        delete = 'ds', -- Using "ds" for muscle memory compat
        find = 'sf', -- Find surrounding (to the right)
        find_left = 'sF', -- Find surrounding (to the left)
        highlight = 'sh', -- Highlight surrounding
        replace = 'cs', -- Replace surrounding -- muscle memory compat
        update_n_lines = 'sn', -- Update `n_lines`

        suffix_last = 'l', -- Suffix to search with "prev" method
        suffix_next = 'n', -- Suffix to search with "next" method
      },
      -- Number of lines within which surrounding is searched
      n_lines = 20,
      -- Whether to respect selection type:
      -- - Place surroundings on separate lines in linewise mode.
      -- - Place surroundings on each line in blockwise mode.
      respect_selection_type = false,
      -- How to search for surrounding (first inside current line, then inside
      -- neighborhood). One of 'cover', 'cover_or_next', 'cover_or_prev',
      -- 'cover_or_nearest', 'next', 'prev', 'nearest'. For more details,
      -- see `:h MiniSurround.config`.
      search_method = 'cover',
      -- Whether to disable showing non-error feedback
      silent = false,
    },
  },
  { "echasnovski/mini.splitjoin",
    opts = {
      mappings = { -- both n/v modes
        toggle = 'gS',
        split = '',
        join = '',
      },
      detect = {
        -- Array of Lua patterns to detect region with arguments.
        -- Default: { '%b()', '%b[]', '%b{}' }
        brackets = nil,
        -- String Lua pattern defining argument separator
        separator = ',',
        -- Array of Lua patterns for sub-regions to exclude separators from.
        -- Enables correct detection in presence of nested brackets and quotes.
        -- Default: { '%b()', '%b[]', '%b{}', '%b""', "%b''" }
        exclude_regions = nil,
      },
      -- Split options
      split = {
        hooks_pre = {},
        hooks_post = {},
      },
      -- Join options
      join = {
        hooks_pre = {},
        hooks_post = {},
      },
    }
  },
  { "echasnovski/mini.trailspace",
    config = true,
    enabled = false,  -- it doesn't seem to work, I've switched to normal `match` highlights instead
  },
  { "echasnovski/mini.clue",
    enabled = false,
    config = function()
      local miniclue = require "mini.clue"
      miniclue.setup {
        triggers = {
          { mode = 'n', keys = '<Leader>' },
          { mode = 'x', keys = '<Leader>' },
          { mode = 'n', keys = 'g' },
          { mode = 'x', keys = 'g' },
          -- Marks
          { mode = 'n', keys = "'" },
          { mode = 'n', keys = '`' },
          { mode = 'x', keys = "'" },
          { mode = 'x', keys = '`' },
          -- Registers
          { mode = 'n', keys = '"' },
          { mode = 'x', keys = '"' },
          { mode = 'i', keys = '<C-r>' },
          { mode = 'c', keys = '<C-r>' },

          { mode = 'n', keys = '<C-w>' },
          { mode = 'n', keys = 'z' },
          { mode = 'x', keys = 'z' },
        },

        clues = {
          -- Enhance this by adding descriptions for <Leader> mapping groups
          miniclue.gen_clues.builtin_completion(),
          miniclue.gen_clues.g(),
          miniclue.gen_clues.marks(),
          miniclue.gen_clues.registers(),
          miniclue.gen_clues.windows(),
          miniclue.gen_clues.z(),
        },
        window = {
          config = {
            width = 50,
          },
          delay = 800,
          scroll_down = '<C-d>',
          scroll_up = '<C-u>',
        },
      }
    end
  }
}
