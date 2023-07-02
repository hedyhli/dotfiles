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
  syntax on
]]

require('lazy').setup('plugin_spec', {
  install = {
    colorscheme = { "dracula" },
  },
})
