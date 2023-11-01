local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
vim.opt.completeopt = { "menuone", "noinsert", "noselect", "preview" }

-- Fix when ft = blah included in plugin spec, syntax not loaded.
vim.cmd [[
  filetype plugin on
  filetype indent on
]]

require("lazy").setup("plugin_spec", {
  install = {
    colorscheme = { "tundra" },
  },
  change_detection = {
    -- I still have to quit and reopen nvim after a new plugin install anyway,
    -- so this feature is useless and annoying.
    enabled = false,
    notify = false,
  },
  ui = {
    -- a number <1 is a percentage., >1 is a fixed size
    size = { width = 1, height = 1 },
    icons = {
      cmd = " ",
      config = "",
      event = "",
      ft = " ",
      init = " ",
      import = " ",
      keys = " ",
      lazy = "󰒲 ",
      loaded = "●",
      not_loaded = "○",
      plugin = " ",
      runtime = "",
      require = "󰢱 ",
      source = " ",
      start = "",
      task = "✔ ",
      list = {
        "●",
        "➜",
        "★",
        "‒",
      },
    },
  },
})
