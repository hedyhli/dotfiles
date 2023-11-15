local function config()
require('nvim-autopairs').setup({
  enable_check_bracket_line = false,   -- Don't close pair when next char is a closing pair
  ignored_next_char = "[%w%.]",        -- will ignore alphanumeric and `.` symbol
  fast_wrap = {},
})
end

-- This plugin below is really good, but whenever I'm on a commented line,
-- press o, press backspace, the line below is joined up above. You won't
-- believe how long it took me to debug this problem and finally realize it's
-- because of this plugin.
-- "jiangmiao/auto-pairs",
--
-- So now I'm using this instead:
--    windwp/nvim-autopairs - extremely customizable, written in lua -
--    integrates wth hrsh7th/nvim-cmp
-- For nvim < 0.5, use: townk/vim-autoclose
return {
  "windwp/nvim-autopairs",
  commit = vim.fn.has("nvim-0.7") == 1 and "b7672cd",
  config = config,
  event = "InsertEnter",
}
