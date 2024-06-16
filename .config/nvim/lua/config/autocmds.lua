local au = function(...) vim.api.nvim_create_autocmd(...) end
-- Return to last edit position when opening files
au("BufReadPost", {
	pattern = "*",
	command = [[if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]]
})

-- File templates
-- VIMHOME is set in init.lua.
au("BufNewFile", {
  pattern = "*.*",
  command = "silent! execute '0r '.$VIMHOME.'/templates/skeleton.'.expand(\"<afile>:e\")"
})

au("BufEnter", {
  pattern = "*",
  command = "set cursorline! | set cursorline!"
})

vim.filetype.add({
  extension = {
    vto = "vento",
  }
})
vim.filetype.add({
  extension = {
    v = "v",
  },
  filename = {
    ['v.mod'] = "v",
  }
})
vim.filetype.add({
  filename = {
    ['build.zig.zon'] = "zig",
  }
})
vim.filetype.add({
  filename = {
    ['dune'] = "lisp",
    ['dune-project'] = "lisp",
  }
})
vim.filetype.add({
  filename = {
    ['justfile'] = "just",
  }
})
