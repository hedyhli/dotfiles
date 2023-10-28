-- Used by Lazy.nvim in loadlazy.lua
return {
  { "dracula/vim",
    name = "dracula", lazy = false, priority = 10000,
    config = function() vim.cmd("colorscheme dracula") end,
    enabled = false, -- LOL I've switched to tundra semi-temporarily
  },
  -- The thing about using dracula theme in nvim here is the annoyance of
  -- having to use lua APIs to interact with something that was designed for
  -- vimscript. With "modern" nvim plugins where the themes are written in 100%
  -- lua, configuring everything else (such as statusline colors) reusing
  -- values defined in these themes provide a much smoother, cleaner
  -- configuration.
  --
  -- This is by no means a /political/ (ahem) statement suggesting my
  -- abandoning of the holy dracula theme. I just, well, decided to try
  -- something new since I sort of realized there is no "perfect" or "best"
  -- theme.
  --
  -- After hours of blood, pain, and tears of trying to tweak dracula colors to
  -- be easier on the eyes and have greater contrast, I took one look at the
  -- carousel of themes from NvChad and fell in love with tundra...
  { "sam4llis/nvim-tundra",
    config = function () require("plugins/tundra") end,
  },
  -- Use 'gf' with dot.separated.modules in lua.
  -- Put the cursor at the argument in "require" and press "gf" to see the effect.
  "sam4llis/nvim-lua-gf",
  { "nvim-tree/nvim-tree.lua",
    cmd = {"NvimTreeOpen", "NvimTreeToggle", "NvimTreeFindFile", "NvimTreeFocus", "NvimTreeFindFileToggle"},
    keys = { "<leader>e" },
    config = function ()
      vim.keymap.set("n", "<leader>e", "<cmd>NvimTreeToggle<cr>",
        { desc = "NvimTreeToggle" })
      require("nvim-tree").setup {
        view = { width = 20, },
        -- renderer = { group_empty = true, },
        -- filters = { dotfiles = true, },
      }
    end,
  },
  -- "jreybert/vimagit",  -- emacs' magit ✨
  -- So anyways apparently almost every plugin that was popular in vim had to
  -- be rewritten in lua.
  { "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",        -- optional
      -- "ibhagwan/fzf-lua",           -- optional
    },
    opts = {
      integrations = {
        -- Use telescope for menu selection rather than vim.ui.select.
        -- Allows multi-select and some things that vim.ui.select doesn't.
        telescope = true,
        -- The diffview integration enables the diff popup.
        diffview = true,
      },
    }
  },
  -- "tpope/vim-surround",  -- quoting and parenthesizing manipulation
  -- "tpope/vim-commentary", -- I'd rather not clog <Leader> mappings with nerd commentor
  -- Sorry tpope! It's more convenient to configure in lua than in vim...
  --   * nvim-comment is lighter but doesn't, AFAIK, support block comments.
  --   * mini.surround let's you add quotes easily (defined above)
  { "numToStr/Comment.nvim", opts = {} },
  { "lewis6991/gitsigns.nvim",
    -- "Be the diff you wish see in the gutter" -- Doom Emacs
    -- Also see comparison with vim-gitgutter:
    -- https://github.com/lewis6991/gitsigns.nvim?tab=readme-ov-file#comparison-with-vim-gitgutter
    enabled = vim.fn.has("nvim-0.8") == 1,
    opts = {
      -- NO WAY! ❤️
      yadm = { enable = true },
    },
  },
  { "folke/trouble.nvim",
    enabled = vim.fn.has("nvim-0.7.2") == 1,
    cmd = "Trouble",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      signs = {
        -- icons / text used for a diagnostic
        error = "E:",
        warning = "W:",
        hint = "H:",
        information = "I:",
        other = "X:",
      },
      fold_open = "", -- icon used for open folds
      fold_closed = "", -- icon used for closed folds
    },
  },
  { "simrat39/symbols-outline.nvim",
    -- Switched to this from tagbar because it does not require exctags, and it
    -- lists the items in order as defined in source code.
    enabled = vim.fn.has("nvim-0.7") == 1,
    cmd = "SymbolsOutline",
    keys = { "<leader>tt" },
    config = function()
      vim.keymap.set("n", "<leader>tt", "<cmd>SymbolsOutline<CR>",
        { desc = "SymbolsOutline" })
      require("symbols-outline").setup {
        symbols = {
          File = { icon = "", hl = "@text.uri" },
          Module = { icon = "M", hl = "@namespace" },
          Namespace = { icon = "N", hl = "@namespace" },
          Package = { icon = "", hl = "@namespace" },
          Class = { icon = "c", hl = "@type" },
          Method = { icon = "m", hl = "@method" },
          Property = { icon = "", hl = "@method" },
          Field = { icon = "F", hl = "@field" },
          Constructor = { icon = "", hl = "@constructor" },
          Enum = { icon = "ℰ", hl = "@type" },
          Interface = { icon = "I", hl = "@type" },
          Function = { icon = "", hl = "@function" },
          Variable = { icon = "α", hl = "@constant" },
          Constant = { icon = "", hl = "@constant" },
          String = { icon = "\"", hl = "@string" },
          Number = { icon = "#", hl = "@number" },
          Boolean = { icon = "", hl = "@boolean" },
          Array = { icon = "A", hl = "@constant" },
          Object = { icon = "⦿", hl = "@type" },
          Key = { icon = "🔐", hl = "@type" },
          Null = { icon = "NULL", hl = "@type" },
          EnumMember = { icon = "", hl = "@field" },
          Struct = { icon = "𝓢", hl = "@type" },
          Event = { icon = "", hl = "@type" },
          Operator = { icon = "+", hl = "@operator" },
          TypeParameter = { icon = "𝙏", hl = "@parameter" },
          Component = { icon = "C", hl = "@function" },
          Fragment = { icon = "g", hl = "@constant" },
        },
      }
    end,
  },
  -- "bling/vim-bufferline", -- I prefer this over taking over the tabline space thanks
  -- Fair well vim-bufferline! You have served my vim and nvim experience well.
  -- I've since decided to set cmdheight to 1 and put the buffers in my status
  -- bar :')
  -- See plugins/statusline.lua.
  -- TODO: Check if this is still needed
  "tpope/vim-endwise",     -- Add those 'endif'/'fi'/'done'
  { "moll/vim-bbye",       -- smart buffer closer
    config = function()
      local function d(s) return { desc = s } end
      local function map(...) vim.keymap.set(...) end
      map("n", "<Leader>bd", "<cmd>Bdelete<cr>", d"Bdelete")
      map("n", "<Leader>bx", "<cmd>Bwipeout<cr>", d"like bd but removes from jumplist")
    end
  },
  { "nvim-tree/nvim-web-devicons", lazy = true,
    enabled = vim.fn.has("nvim-0.7") == 1,
    config = function() require('plugins/icons') end,
    pin = true,
    commit = 'cde67b5d5427daeecfd7c77cf02ded23a26980bb',
  },
  { -- STATUS LINE
    -- TODO: Switch to heir for more customizability
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function() require("plugins/statusline") end,
  },
  { "tiagovla/scope.nvim",
    -- Tab-local buffer list.
    -- Similar to beframe (by prot) from emacs!
    config = true
  },
  { 'nvim-telescope/telescope.nvim', tag = '0.1.4',
    dependencies = { 'nvim-lua/plenary.nvim' },
    enabled = vim.fn.has("nvim-0.9") == 1,
    config = function() require('plugins/telescope') end,
  },
  { "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      highlight = {
        multiline = false,
        -- The 3 lines below tunes down the coloring of the highlights.
        -- The color schemes and treesitter of nowdays are already way too
        -- vibrant for my liking, and for someone who litters todo/fixme way
        -- too much everywhere, adding *wide* *bg* highlighting to them is just
        -- WAY TOO MUCH EYE CANDY AAAA
        before = "",
        keyword = "fg", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty.
        after = "", -- "fg" or "bg" or empty

        pattern = [[.*<(KEYWORDS)\s*]], -- (vim regex)
        comments_only = true, -- uses treesitter to match keywords in comments only
        max_line_len = 200,
        exclude = {}, -- filetypes
      },
    }
  },
  { "echasnovski/mini.files",
    -- Neat popout window to browse dirs and manipulate within the editor!
    cmd = "MiniFiles",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require('mini.files').setup()
      vim.api.nvim_create_user_command('MiniFiles', function()
        MiniFiles.open()
      end, { desc = "Calls lua MiniFiles.open()" })
    end,
  },
  { "echasnovski/mini.surround", version = '*',
    opts = {
        -- Duration (in ms) of highlight when calling `MiniSurround.highlight()`
        highlight_duration = 500,
        -- Module mappings. Use `''` (empty string) to disable one.
        mappings = {
          add = 'sa', -- Add surrounding in Normal and Visual modes
          delete = 'ds', -- Delete surrounding -- Using "ds" for muscle memory compat
          find = 'sf', -- Find surrounding (to the right)
          find_left = 'sF', -- Find surrounding (to the left)
          highlight = 'sh', -- Highlight surrounding
          replace = 'cs', -- Replace surrounding -- muscle memory compat
          update_n_lines = 'sn', -- Update `n_lines`

          suffix_last = 'l', -- Suffix to search with "prev" method
          suffix_next = 'n', -- Suffix to search with "next" method
        },
        -- Number of lines within which surrounding is searched
        n_lines = 20,
        -- Whether to respect selection type:
        -- - Place surroundings on separate lines in linewise mode.
        -- - Place surroundings on each line in blockwise mode.
        respect_selection_type = false,
        -- How to search for surrounding (first inside current line, then inside
        -- neighborhood). One of 'cover', 'cover_or_next', 'cover_or_prev',
        -- 'cover_or_nearest', 'next', 'prev', 'nearest'. For more details,
        -- see `:h MiniSurround.config`.
        search_method = 'cover',
        -- Whether to disable showing non-error feedback
        silent = false,
      },
  },
  { "lukas-reineke/indent-blankline.nvim",
    -- https://github.com/lukas-reineke/indent-blankline.nvim/wiki/Migrate-to-version-3
    -- Not documented but it appears new version requires nvim nightly.
    -- Upgraded to nvim 0.9.4 and the new version worked with showing context
    -- without needing treesitter! 🎉
    main = vim.fn.has("nvim-0.9") == 1 and "ibl",
    -- Use the 2 lines below if the nvim version is not supported
    version = vim.fn.has("nvim-0.9") == 0 and "2.20.8",
    pin = vim.fn.has("nvim-0.9") == 0,
    opts = {
      exclude = {
        filetypes = {
          "help",
          "NvimTree",
          "Trouble",
          "trouble",
          "lazy",
          "mason",
          "notify",
          "toggleterm",
          "lazyterm",
        },
      },
    },
  },
  { "RRethy/vim-illuminate",
    config = function ()
			require("illuminate").configure{}
			vim.cmd([[hi def IlluminatedWordText guibg=Gray]])
		end,
		enabled = false, -- Somehow wasn't working for me
  },

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
    config = function ()
      vim.opt_local.textwidth = 80
    end
  },
  { "leafo/moonscript-vim", ft = "moon",
    config = function ()
      vim.opt_local.tabstop = 2
      vim.opt_local.softtabstop = 2
      vim.opt_local.shiftwidth = 2
    end
  },

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
  { "mfussenegger/nvim-lint",
    enabled = vim.fn.has("nvim-0.6") == 1,
    config = function() require("plugins/linting") end,

  },
  { "windwp/nvim-autopairs",
    commit = vim.fn.has("nvim-0.7") == 1 and "b7672cd",
    config = function() require("plugins/autopair") end,
  },

  -- Best IDE autocomplete setup ever
  { "neovim/nvim-lspconfig",
    config = function()
      require('plugins/lsp')
    end
  },
  { "ray-x/lsp_signature.nvim",
    event = "VeryLazy",
    config = function(_, opts) require'lsp_signature'.setup(opts) end,
    opts = {
      doc_lines = 10,
      -- set to 0 if you DO NOT want any API comments be shown
      -- This setting only take effect in insert mode, it does not affect signature help in normal
      -- mode, 10 by default
      max_height = 12,
      max_width = 80,
      noice = false,
      wrap = true,
      floating_window = false,
      floating_window_above_cur_line = true,
      floating_window_off_x = 1,
      floating_window_off_y = 0, -- -2 move window up 2 lines; 2 move down 2 lines
      -- can be either number or function, see examples
      close_timeout = 4000,
      fix_pos = false,  -- don't auto-close the floating window all parameters finished
      hint_enable = true, -- virtual hint
      hint_prefix = ": ",
      hint_scheme = "String",
      hint_inline = function() return vim.fn.has('nvim-0.10') == 1 end,
      hi_parameter = "LspSignatureActiveParameter",
      handler_opts = { border = "rounded" },
      always_trigger = false,
      auto_close_after = nil,
      extra_trigger_chars = {","},
      zindex = 200,
      padding = '',
      transparency = nil, -- 1~100
      timer_interval = 200, -- lower to reduce latency
      toggle_key = '<M-s>', -- toggle floating window key (must set below to true)
      toggle_key_flip_floatwin_setting = true,
      select_signature_key = '<M-n>', -- next signature for (eg) overloads
    },
  },
  -- Treesitter!
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate",
    version = false,
    event = "VeryLazy",
    cmd = { "TSUpdateSync", "TSUpdate", "TSInstall", "Inspect", "InspectTree" },
    config = function() require("plugins/treesitter") end,
  },
  { "nvim-treesitter/nvim-treesitter-context",
    dependencies = "nvim-treesitter/nvim-treesitter",
    after = "nvim-treesitter",
  },
  { "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = "nvim-treesitter/nvim-treesitter",
    after = "nvim-treesitter",
  },
  { "windwp/nvim-ts-autotag",
    -- Automatically add closing tags for HTML and JSX
    ft = { "html", "tsx", "jsx" },
    opts = {},
  },
  -- End of treesitter
  -- Begin completion framework
  -- Please see ./lua/plugins/complete.lua
  { "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdlineEnter" },
    config = function() require('plugins/complete') end,
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      -- "hrsh7th/cmp-buffer",     -- Completions of words in current buffer
      "hrsh7th/cmp-path",       -- File paths
      "hrsh7th/cmp-cmdline",    -- Fire your way through the neovim cmd line
      "hrsh7th/cmp-calc",       -- Somehwat useful.. But emacs M-x calc FTW!
      "hrsh7th/cmp-emoji",      -- 😏 :smirk:
      "mtoohey31/cmp-fish",
      "petertriho/cmp-git",
      "kdheepak/cmp-latex-symbols", -- τ long live \tau
      -- "dcampos/nvim-snippy",
      "dcampos/cmp-snippy",
    },
  },
  { "dcampos/nvim-snippy",
    opts = {
      -- mappings to navigate expansion fields are merged in plugins/complete.lua
      mappings = {
        is = {},
        nx = {
          ['<leader>sx'] = 'cut_text',
        },
      },
    }
  },
  { "onsails/lspkind.nvim",
  },
  { "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function()
      require("lsp_lines").setup()

      vim.diagnostic.config({ virtual_lines = false })
      vim.api.nvim_create_autocmd("BufReadPost", {
        pattern = "*",
        callback = function()
          vim.b.lsp_lines_enabled = false
        end
      })

      vim.keymap.set("n", "<Leader>ll", function()
        require("lsp_lines").toggle()
        -- Disable virtual_text since it's redundant due to lsp_lines.
        if vim.b.lsp_lines_enabled then
          -- IT was enabled, now it's disabled.
          vim.diagnostic.config({ virtual_text = true })
          vim.b.lsp_lines_enabled = false
        else
          vim.diagnostic.config({ virtual_text = false })
          vim.b.lsp_lines_enabled = true
        end
      end , {desc = "Toggle Lsp Lines"})
    end,
  },
  { "xeluxee/competitest.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    config = function() require("plugins/competitest") end,
  },
  { "nvim-neorg/neorg",
    -- Org... and better? I sure hope so!
    cmd = "Neorg",
    ft = "norg",
    build = ":Neorg sync-parsers",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("neorg").setup {
        load = {
          ["core.defaults"] = {}, -- Loads default behaviour
          ["core.concealer"] = {}, -- Adds pretty icons to your documents
          ["core.dirman"] = { -- Manages Neorg workspaces
            config = {
              workspaces = { neorg = "~/neorg" },
              default_workspace = "neorg"
            },
          },
        },
      }
    end,
  },
  { "folke/which-key.nvim",
    -- The most mind blowing steal from ever

    event = "VeryLazy",
    enabled = false,
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 600
    end,
    opts = {
      plugins = {
        marks = true, -- shows a list of your marks on ' and `
        registers = false, -- Slightly annoying
        -- Although it reminds me of my calculator giving me a similar preview
        -- of stored variables upon the "recall" key haha
        spelling = {
          enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
          suggestions = 20, -- how many suggestions should be shown in the list?
        },
        presets = {
          operators = true, -- adds help for operators like d, y, ...
          motions = true, -- adds help for motions
          text_objects = true, -- help for text objects triggered after entering an operator
          windows = true, -- default bindings on <c-w>
          nav = true, -- misc bindings to work with windows
          z = true, -- bindings for folds, spelling and others prefixed with z
          g = true, -- bindings for prefixed with g
        },
      },
      -- add operators that will trigger motion and text object completion
      -- to enable all native operators, set the preset / operators plugin above
      operators = { gc = "Comments" },
      key_labels = {
        -- ["<space>"] = "SPC",
        -- ["<cr>"] = "RET",
        -- ["<tab>"] = "TAB",
      },
      motions = { count = true, },
      icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "=", -- symbol used between a key and it's label
        group = "+", -- symbol prepended to a group
      },
      popup_mappings = {
        scroll_down = "<c-d>", -- binding to scroll down inside the popup
        scroll_up = "<c-u>", -- binding to scroll up inside the popup
      },
      window = {
        border = "shadow", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = { 0, 0, 0, 0 }, -- extra window margin [top, right, bottom, left]. When between 0 and 1, will be treated as a percentage of the screen size.
        padding = { 0, 1, 0, 1 }, -- extra window padding [top, right, bottom, left]
        winblend = 0, -- value between 0-100 0 for fully opaque and 100 for fully transparent
        zindex = 1000, -- positive value to position WhichKey above other floating windows.
      },
      layout = {
        height = { min = 4, max = 20 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 2, -- spacing between columns
        align = "left", -- align columns left, center or right
      },
      ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
      -- hide mapping boilerplate
      hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "^:", "^ ", "^call ", "^lua " },
      show_help = true, -- show a help message in the command line for using WhichKey
      show_keys = true, -- show the currently pressed key and its label as a message in the command line
      triggers = "auto", -- automatically setup triggers
      -- triggers = {"<leader>"} -- or specifiy a list manually
      -- list of triggers, where WhichKey should not wait for timeoutlen and show immediately
      triggers_nowait = {
        -- marks
        "`",
        "'",
        "g`",
        "g'",
        -- registers
        -- '"',
        -- "<c-r>",
        -- spelling
        "z=",
      },
      triggers_blacklist = {
        -- list of mode / prefixes that should never be hooked by WhichKey
        -- this is mostly relevant for keymaps that start with a native binding
        i = { "j", "k" },
        v = { "j", "k" },
      },
      -- disable the WhichKey popup for certain buf types and file types.
      -- Disabled by default for Telescope
      disable = {
        buftypes = {},
        filetypes = {},
      },
    }
  },
  "folke/neodev.nvim",
}
