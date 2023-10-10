vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[;]]
-- The cool thing about ';' is that it is at the same
-- position as ':', the char used for commands.

-- Set envvar VIMHOME to, eg, ~/.config/nvim (has no trailing slash)
vim.env.VIMHOME = vim.fn.expand('<sfile>:p:h')

require('loadlazy')

require('general')
require('mappings')
require('autocmds')
