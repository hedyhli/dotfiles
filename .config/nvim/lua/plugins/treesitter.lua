
local configs = require("nvim-treesitter.configs")
configs.setup({
  ensure_installed = {
    "cpp", "lua", "vim", "vimdoc", "html", "go", "bash", "regex", "markdown", "markdown_inline", "query", "toml", "vimdoc", "python", "diff",
  },
  sync_install = false,
  highlight = { enable = true },
  indent = { enable = true },
  -- Textobjects, see below as well
  textobjects = {
    select = {
      enable = true,
      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = { query = "@function.outer", desc = "Around function" },
        ["if"] = { query = "@function.inner", desc = "Inside function" },
        ["ac"] = { query = "@class.outer", desc = "Around class" },
        ["ic"] = { query = "@class.inner", desc = "Inside class" },
        -- You can also use captures from other query groups like `locals.scm`
        ["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
      },
      include_surrounding_whitespace = false,
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next = {
        ["]p"] = "@parameter.outer",
      },
      goto_previous = {
        ["[p"] = "@parameter.outer",
      },
    }
  },
})

-- TS context
require("treesitter-context").setup {
  enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
  max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
  min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
  line_numbers = true,
  multiline_threshold = 20, -- Maximum number of lines to show for a single context
  trim_scope = 'outer', -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
  mode = 'topline',  -- Line used to calculate context. Choices: 'cursor', 'topline'
  -- Separator between context and content. Should be a single character string, like '-'.
  -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
  separator = nil,
  zindex = 20, -- The Z-index of the context window
  on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
}

--- TS textobjects ---
vim.keymap.set("n", "].", function()
  local t = require("nvim-treesitter.ts_utils")
  local node = t.get_node_at_cursor()
  if node ~= nil then
    t.goto_node(t.get_next_node(node, true, true), false, true)
  end
end,
  { desc = "Go to next sibling node" }
)
vim.keymap.set("n", "[.", function()
  local t = require("nvim-treesitter.ts_utils")
  local node = t.get_node_at_cursor()
  if node ~= nil then
    t.goto_node(t.get_previous_node(node, true, true), false, true)
  end
end,
  { desc = "Go to previous sibling node" }
)

-- Textobject setup{} opts were set at the top along with treesitter.
-- Allow repeat keys
local ts_repeat_move = require "nvim-treesitter.textobjects.repeatable_move"
-- ensure ; goes forward and , goes backward regardless of the last direction
vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move_next)
vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_previous)
-- vim way: ; goes to the direction you were moving.
-- vim.keymap.set({ "n", "x", "o" }, ";", ts_repeat_move.repeat_last_move)
-- vim.keymap.set({ "n", "x", "o" }, ",", ts_repeat_move.repeat_last_move_opposite)
-- Optionally, make builtin f, F, t, T also repeatable with ; and ,
vim.keymap.set({ "n", "x", "o" }, "f", ts_repeat_move.builtin_f)
vim.keymap.set({ "n", "x", "o" }, "F", ts_repeat_move.builtin_F)
vim.keymap.set({ "n", "x", "o" }, "t", ts_repeat_move.builtin_t)
vim.keymap.set({ "n", "x", "o" }, "T", ts_repeat_move.builtin_T)
