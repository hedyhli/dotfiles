-- Used by Lazy.nvim in loadlazy.lua
return {
  { "dracula/vim", name = "dracula", init = function()
    -- THE most important nvim configuration
    vim.cmd("colorscheme dracula")
  end },
  {
    "nvim-tree/nvim-tree.lua",
    config = function()
      require("nvim-tree").setup({
        view = { width = 20, },
        -- renderer = { group_empty = true, },
        -- filters = { dotfiles = true, },
      })
    end
  },
  "tpope/vim-fugitive",
  "jreybert/vimagit",  -- emacs' magit ✨
  "tpope/vim-surround",  -- quoting and parenthesizing manipulation
  "tpope/vim-commentary", -- I'd rather not clog <Leader> mappings with nerd commentor
  "airblade/vim-gitgutter", -- Show git diff overview stuff in the left column
  { -- Quickly jump to a symbol in buffer (one of my most used). Requires exuberant ctags
    "majutsushi/tagbar",
    config = function()
      vim.keymap.set("n", "<leader>tt", "<cmd>TagbarToggle<CR>")
      vim.g.tagbar_width = 20
    end,
  },
  "bling/vim-bufferline", -- I prefer this over taking over the tabline space thanks
  "tpope/vim-endwise",     -- Add those 'endif'/'fi'/'done'
  { "moll/vim-bbye",       -- smart buffer closer
  config = function()
    vim.keymap.set("n", "<Leader>bd", "<cmd>Bdelete<cr>")
    vim.keymap.set("n", "<Leader>bx", "<cmd>Bwipeout<cr>") -- like bd but removes from jumplist
  end
  },
  {
    "nvim-tree/nvim-web-devicons", lazy = true,
    enabled = vim.fn.has("nvim-0.7") == 1,
    config = function() require('plugins/icons') end,
  },
  { -- STATUS LINE
    "nvim-lualine/lualine.nvim",
    dependencies = { 'nvim-tree/nvim-web-devicons', opt = true },
    config = function() require('plugins/statusline') end,
  },
  {  -- Neat popout window to browse dirs and manipulate within the editor!
    "echasnovski/mini.files",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function() require('mini.files').setup() end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({
        -- These require treesitter
        -- show_current_context = true,
        -- show_current_context_start = true,
      })
    end,
  },

  --- File type, syntax, language helper plugins ---
  {
    url = "https://git.sr.ht/~torresjrjr/gemini.vim",
    -- ft = "gemini",
  },
  { "cespare/vim-toml", },
  { "blankname/vim-fish", },
  {
    url = "https://git.rawtext.club/slope-lang/slope-vim-syntax",
    -- ft = "slope",
  },
  { "mzlogin/vim-markdown-toc", },
  { "leafo/moonscript-vim", },

  ------ LSP and autopair plugins ------
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
  "nvim-lua/plenary.nvim",  -- Helper lua functions many plugins depend on
  {
    "mfussenegger/nvim-lint", enabled = vim.fn.has("nvim-0.6") == 1,
    config = function() require("plugins/linting") end,

  },
  { "windwp/nvim-autopairs", commit = vim.fn.has("nvim-0.7") == 1 and "b7672cd",
    config = function() require("plugins/autopair") end,
  },

  -- Best IDE autocomplete setup ever
  -- Please see ./lua/complete.lua
  { "neovim/nvim-lspconfig", config = function() require('plugins/lsp') end },
  { "hrsh7th/cmp-nvim-lsp", dependencies = "neovim/nvim-lspconfig" },
  -- Completions of words in current buffer
  { "hrsh7th/cmp-buffer", dependencies = "hrsh7th/nvim-cmp" },
  -- File paths
  { "hrsh7th/cmp-path", dependencies = "hrsh7th/nvim-cmp" },
  -- Fire your way through the neovim cmd line
  { "hrsh7th/cmp-cmdline", dependencies = "hrsh7th/nvim-cmp" },
  { "hrsh7th/cmp-calc", dependencies = "hrsh7th/nvim-cmp" },
  -- 😏 :smirk:
  { "hrsh7th/cmp-emoji", dependencies = "hrsh7th/nvim-cmp" },
  { "mtoohey31/cmp-fish", dependencies = "hrsh7th/nvim-cmp" },
  { "petertriho/cmp-git", dependencies = "hrsh7th/nvim-cmp" },
  -- τ long live \tau
  { "kdheepak/cmp-latex-symbols", dependencies = "hrsh7th/nvim-cmp" },
  { "dcampos/nvim-snippy", enabled = vim.fn.has("nvim-0.7") == 1 },
  { "dcampos/cmp-snippy",
    enabled = vim.fn.has("nvim-0.7") == 1,
    dependencies = {"hrsh7th/nvim-cmp", "dcampos/nvim-snippy"},
  },
  { "hrsh7th/nvim-cmp", config = function() require('plugins/complete') end },

  "onsails/lspkind.nvim",  -- Symbols in the completion
  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()
      vim.diagnostic.config({ virtual_lines = false })
    end,
  },
}
