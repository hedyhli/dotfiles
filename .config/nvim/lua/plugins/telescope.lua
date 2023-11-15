local function config()

local function d(s) return { desc = s } end
local function map(...) vim.keymap.set(...) end

-- Go to dir of selected entry
local goto_dir = function(prompt_bufnr)
  local selection = require("telescope.actions.state").get_selected_entry()
  if selection == nil then
    print("No selection!")
    return
  end
  local dir = vim.fn.fnamemodify(selection.path, ":p:h")
  require("telescope.actions").close(prompt_bufnr)
  local cmd = string.format("silent lcd %s", dir)
  vim.cmd(cmd)
  print(cmd)
end

-- Open picker again but now in parent dir.
-- Set noshowmode to prevent printed messages from being buried by "--INSERT--"
local reprompt_from_parent_dir = function(picker, prompt_bufnr)
  -- local selection = require("telescope.actions.state").get_selected_entry()
  -- if selection == nil then
  --   print("No selection!")
  --   return
  -- end
  -- local dir = vim.fn.fnamemodify(selection.path, ":s?/.*$??:p:h:h")
  -- Let selection.path = ".config/nvim/init.lua"
  --     PWD = ~/.config/
  -- What fnamemodify does:
  -- Result                     After operation
  -- ------                     ---------------
  -- "nvim/init.lua"
  -- "nvim"                     :s?/.*$??
  -- "/home/user/.config/nvim"  :p
  -- "/home/user/.config"       :h
  -- "/home/user"               :h
  -- Hence it changes cwd of picker from ~/.config/ to ~/
  require("telescope.actions").close(prompt_bufnr)
  vim.cmd("cd ..")
  require("telescope.builtin")[picker]()
  print("Using "..vim.fn.getcwd())
end

-- Open picker again but now in git root if available.
-- Set noshowmode to prevent printed messages from being buried by "--INSERT--"
local reprompt_from_git_root = function(picker, prompt_bufnr)
  vim.fn.system({"git", "rev-parse", "--is-inside-work-tree"})
  if vim.v.shell_error == 0 then
    local opts = {
      cwd = vim.fn.fnamemodify(vim.fn.finddir(".git", ".;"), ":h"),
    }
    require("telescope.actions").close(prompt_bufnr)
    require("telescope.builtin")[picker](opts)
    print("Using "..opts.cwd)
  else
    print("Failed to find git root")
  end
end

map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", d"Telescope find_files")
map("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", d"Telescope oldfiles")
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", d"Telescope live_grep")
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", d"Telescope buffers")
map("n", "<leader>bb", "<cmd>Telescope buffers<cr>", d"Telescope buffers")
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", d"Telescope help_tags")
map("n", "<leader>fR", "<cmd>Telescope registers<cr>", d"Telescope registers (also see <leader>rg)")

require('telescope').setup({
  defaults = {
    -- layout_config = {
      -- vertical = { width = 0.3 },
      -- horizontal = { width = 0.5 },
    -- },
    -- preview = {
    --   filesize_limit = 0.1, -- MB
    -- },
    -- theme = "dropdown",
    mappings = {
      n = {
        ["cd"] = goto_dir,
      },
      i = {
        ["<C-g>"] = "close",
      },
    },
  },
  pickers = {
    find_files = {
      mappings = {
        n = {
          ["<C-d>"] = function(...) return reprompt_from_parent_dir("find_files", ...) end,
          ["<C-S-r>"] = function(...) return reprompt_from_git_root("find_files", ...) end,
        },
      },
    },
    live_grep = {
      mappings = {
        n = {
          ["<C-d>"] = function(...) return reprompt_from_parent_dir("live_grep", ...) end,
          ["<C-S-r>"] = function(...) return reprompt_from_git_root("live_grep", ...) end,
        },
      },
    },
  },
})
require('telescope').load_extension('fzf')
end

return {
  'nvim-telescope/telescope.nvim', tag = '0.1.4',
  enabled = vim.fn.has("nvim-0.9") == 1,
  dependencies = {
    'nvim-lua/plenary.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  },
  config = config,
}
