return {
  --- File type, syntax, language helper plugins ---
  { url = "https://git.sr.ht/~torresjrjr/gemini.vim", ft = "gemini" },
  { "cespare/vim-toml", ft = "toml" },
  { "blankname/vim-fish", ft = "fish",
    config = function ()
      vim.opt_local.shiftwidth = 4
      vim.opt_local.textwidth = 79
      vim.opt_local.foldmethod = "expr"
      vim.opt_local.expandtab = true
      vim.opt_local.tabstop = 4
    end,
  },
  { url = "https://git.rawtext.club/slope-lang/slope-vim-syntax",
    ft = "slope",
    enabled = false;
  },
  { "mzlogin/vim-markdown-toc", ft = "markdown",
  },
  { dir = "~/projects/markdown-toc.nvim", ft = "markdown",
    opts = {
      -- fences = false,
      toc_list = {
        -- indent_size = function() return vim.bo.shiftwidth end,
      }
    },
  },
  { "leafo/moonscript-vim", ft = "moon",
    config = function ()
      vim.opt_local.tabstop = 2
      vim.opt_local.softtabstop = 2
      vim.opt_local.shiftwidth = 2
    end
  },
}
