require('nvim-autopairs').setup({
  enable_check_bracket_line = false,   -- Don't close pair when next char is a closing pair
  ignored_next_char = "[%w%.]",        -- will ignore alphanumeric and `.` symbol
  fast_wrap = {},
})
