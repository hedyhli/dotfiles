vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[;]]
-- The cool thing about ';' is that it is at the same
-- position as ':', the char used for commands.

require('loadlazy')

require('general')
require('mappings')
require('autocmds')
