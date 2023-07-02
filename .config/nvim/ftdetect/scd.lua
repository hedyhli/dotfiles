vim.api.nvim_create_autocmd(
  { "BufNewFile", "BufRead" },
  { pattern = "*.scd", command = "set filetype=scdoc" }
)
