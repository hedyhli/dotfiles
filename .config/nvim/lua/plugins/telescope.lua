local function d(s) return { desc = s } end
local function map(...) vim.keymap.set(...) end

map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", d"Telescope find_files")
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>", d"Telescope live_grep")
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", d"Telescope buffers")
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", d"Telescope help_tags")
map("n", "<leader>fR", "<cmd>Telescope registers<cr>", d"Telescope registers (also see <leader>rg)")

require('telescope').setup({
  defaults = {
    layout_config = {
      -- vertical = { width = 0.3 },
      horizontal = { width = 0.5 },
    },
    preview = {
      filesize_limit = 0.1, -- MB
    },
  },
  pickers = {
    find_files = {
      theme = "dropdown",
    },
    mappings = {
      n = {
        ["<C-d>"] = function(prompt_bufnr)
          local selection = require("telescope.actions.state").get_selected_entry()
          local dir = vim.fn.fnamemodify(selection.path, ":p:h")
          require("telescope.actions").close(prompt_bufnr)
          -- Depending on what you want put `cd`, `lcd`, `tcd`
          vim.cmd(string.format("silent lcd %s", dir))
        end
      }
    }
  },
})
