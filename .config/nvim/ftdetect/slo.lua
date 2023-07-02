vim.api.nvim_create_autocmd(
  { "BufNewFile", "BufRead" },
  { pattern = "*.slo", command = "set filetype=slope" }
)
