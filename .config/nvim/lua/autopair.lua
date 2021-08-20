require('nvim-autopairs').setup({
    enable_check_bracket_line = false,   -- Don't close pair when next char is a closing pair
    ignored_next_char = "[%w%.]",        -- will ignore alphanumeric and `.` symbol
})

require("nvim-autopairs.completion.compe").setup({
    map_cr = true, --  map <CR> on insert mode
    map_complete = true, -- it will auto insert `(` after select function or method item
    auto_select = false,  -- auto select first item
})

-- TODO: add fastwrap and other stuff support
