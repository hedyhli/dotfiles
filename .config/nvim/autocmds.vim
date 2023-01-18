if has("nvim-0.6")
  au BufWritePost lua require('lint').try_lint()
endif
