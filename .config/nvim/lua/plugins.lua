-- Used by Lazy.nvim in loadlazy.lua
--
-- These are plugins that don't have their own dedicated config
-- file in plugins/
return {
  -- Use 'gf' with dot.separated.modules in lua.
  -- Put the cursor at the argument in "require" and press "gf" to see the effect.
  "sam4llis/nvim-lua-gf",

  { "nvim-tree/nvim-tree.lua",
    cmd = {"NvimTreeOpen", "NvimTreeToggle", "NvimTreeFindFile", "NvimTreeFocus", "NvimTreeFindFileToggle"},
    keys = { { "<leader>e", "<cmd>NvimTreeToggle<cr>", desc = ":NvimTreeToggle" } },
    config = function ()
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
  { "numToStr/Comment.nvim", config = true },
  { "lewis6991/gitsigns.nvim",
    -- "Be the diff you wish see in the gutter" -- Doom Emacs
    -- Also see comparison with vim-gitgutter:
    -- https://github.com/lewis6991/gitsigns.nvim?tab=readme-ov-file#comparison-with-vim-gitgutter
    enabled = vim.fn.has("nvim-0.8") == 1,
    opts = {
      -- No way ❤️
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
  -- Full disclosure: I'm the maintainer of outline.nvim (see plugins/outline),
  -- just trying out aerial for fun :)
  { 'stevearc/aerial.nvim',
    enabled = true,
    cmd = { "AerialOpen", "AerialToggle", "AerialNavToggle" },
    opts = {
      -- Doesn't seem to work
      filter_kind = false,
    },
    -- Optional dependencies
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },
  },
  -- "bling/vim-bufferline", -- I prefer this over taking over the tabline space thanks
  -- Fair well vim-bufferline! You have served my vim and nvim experience well.
  -- I've since decided to set cmdheight to 1 and put the buffers in my status
  -- bar :')
  -- See archived/statusline.lua.
  -- Now I've abandoned the notion of having to know opened buffers all the
  -- time. If I need it I can use bp/bn/bd and telescope buffers.
  -- TODO: Check if this is still needed
  "tpope/vim-endwise",     -- Add those 'endif'/'fi'/'done'
  { "moll/vim-bbye",       -- smart buffer closer
    config = function()
      local function d(s) return { desc = s } end
      local map = vim.keymap.set
      map("n", "<Leader>bd", "<cmd>Bdelete<cr>", d"Bdelete")
      map("n", "<Leader>bx", "<cmd>Bwipeout<cr>", d"like bd but removes from jumplist")
    end
  },
  { "carbon-steel/detour.nvim" },
  { "tiagovla/scope.nvim",
    -- Tab-local buffer list.
    -- Similar to beframe (by prot) from emacs!
    config = true
  },
  { "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      highlight = {
        multiline = false,
        -- The 3 lines below tunes down the coloring of the highlights. The
        -- color schemes and treesitter nowdays are already way too vibrant for
        -- my liking, and for someone who litters todo/fixme way too much
        -- everywhere, adding *wide* *bg* highlighting to them is just WAY too
        -- much eye candy.
        before = "",
        keyword = "fg", -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty.
        after = "", -- "fg" or "bg" or empty
        -- Require only one space before keyword so we don't match keywords
        -- in the middle of comments.
        pattern = [[\s<(KEYWORDS)\s*]], -- (vim regex)
        comments_only = true, -- uses treesitter to match keywords in comments only
        max_line_len = 200,
        exclude = {}, -- filetypes
      },
    }
  },
  { "lukas-reineke/indent-blankline.nvim", name = "ibl",
    -- https://github.com/lukas-reineke/indent-blankline.nvim/wiki/Migrate-to-version-3
    -- Not documented but it appears new version requires a higher version of nvim.
    -- Upgraded to nvim 0.9.4 and the new version worked with showing context
    -- without needing treesitter 🎉
    main = vim.fn.has("nvim-0.9") == 1 and "ibl",
    version = vim.fn.has("nvim-0.9") == 1 and "*" or "2.20.8",
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
          "Outline",
        },
      },
    },
  },

  { "windwp/nvim-ts-autotag",
    -- Automatically add closing tags for HTML and JSX
    ft = { "html", "tsx", "jsx" },
    opts = {},
  },
  "folke/neodev.nvim",
}
