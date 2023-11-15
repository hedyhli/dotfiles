-- Org... and better? I sure hope so!

return {
  "nvim-neorg/neorg",
  cmd = "Neorg",
  ft = "norg",
  build = ":Neorg sync-parsers",
  dependencies = { "nvim-lua/plenary.nvim", "nvim-treesitter/nvim-treesitter" },
  config = function()
    require("neorg").setup {
      load = {
        ["core.defaults"] = {}, -- Loads default behaviour
        ["core.concealer"] = {
          config = {
            folds = true,
          }
        },
        ["core.dirman"] = { -- Manages Neorg workspaces
          config = {
            workspaces = { neorg = "~/neorg" },
            default_workspace = "neorg"
          },
        },
        ["core.completion"] = {
          config = {
            engine = "nvim-cmp",
            name = " "
          },
        },
        ["core.highlights"] = {
          config = {
            highlights = {
              tags = {
                ranged_verbatim = {
                  parameters = "guifg=#9ca3af",
                  begin = "guifg=#9ca3af",
                  ["end"] = "guifg=#9ca3af",
                  name =  {
                    word = "guifg=#9ca3af",
                  },
                },
              },
            },
          },
        },
      },
    }
  end,
}
