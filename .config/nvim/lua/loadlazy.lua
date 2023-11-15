-- Bootstrap and set up lazy.nvim
-- Lazy.nvim is one of the things to keeps me from emacs 💔
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
vim.opt.completeopt = { "menu", "noinsert", "noselect", "preview" }

-- Fix when ft = blah included in plugin spec, syntax not loaded.
vim.cmd [[
  filetype plugin on
  filetype indent on
]]

require("lazy").setup("plugins", {
  install = {
    colorscheme = { "tundra" },
  },
  change_detection = {
    enabled = true,
    notify = false,
  },
  ui = {
    -- a number <1 is a percentage., >1 is a fixed size
    size = { width = 0.8, height = 0.9 },
    border = "rounded",
  },
})
