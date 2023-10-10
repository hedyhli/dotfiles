vim.g.mapleader = [[ ]]
-- vim.g.maplocalleader = [[;]]
-- See my vim config comment on me being stupid enough to not
-- have known f, t, F, T and ';' 😔

-- Set envvar VIMHOME to, eg, ~/.config/nvim (has no trailing slash)
vim.env.VIMHOME = vim.fn.expand('<sfile>:p:h')

require('loadlazy')

require('general')
require('mappings')
require('autocmds')
