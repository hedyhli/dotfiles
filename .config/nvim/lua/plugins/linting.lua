-- https://github.com/errata-ai/vale/releases
local lint = require('lint')
lint.linters_by_ft = {
  markdown = {'codespell'},
  python = {'codespell'},
  mail = {'proselint'},
  template = {'proselint'},
  c = {'cpplint'},
  -- cpp = {'cpplint'},
}

vim.api.nvim_create_autocmd("BufWritePost", { pattern = "*", callback = function() lint.try_lint() end })
