vim.api.nvim_create_autocmd(
   { "BufNewFile", "BufRead" },
   { pattern = "*.moon", command = "set filetype=moon" }
)
