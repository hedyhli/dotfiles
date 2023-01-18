-- https://github.com/errata-ai/vale/releases
require('lint').linters_by_ft = {
  markdown = {'codespell',},
  lua = {'luacheck',},
  python = {'codespell',},
}
