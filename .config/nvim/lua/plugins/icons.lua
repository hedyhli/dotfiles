-- Remember to install the Symbols Only font zip, then configure terminal to
-- use that font too.
-- Used by lazy, nvim-tree, and others

-- https://github.com/nvim-tree/nvim-web-devicons
local function config()
require'nvim-web-devicons'.setup {
  -- your personnal icons can go here (to override)
  -- you can specify color or cterm_color instead of specifying both of them
  -- DevIcon will be appended to `name`
  --
  -- globally enable different highlight colors per icon (default to true)
  -- if set to false all icons will have the default icon's color
  color_icons = true;
  -- globally enable default icons (default to false)
  -- will get overriden by `get_icons` option
  -- default = true;
  -- globally enable "strict" selection of icons - icon will be looked up in
  -- different tables, first by filename, and if not found by extension; this
  -- prevents cases when file doesn't have any extension but still gets some icon
  -- because its name happened to match some extension (default to false)
  strict = true;
  -- same as `override` but specifically for overrides by filename
  -- takes effect when `strict` is true
  override_by_filename = {
    [".gitignore"] = {
      icon = "",
      color = "#f1502f",
      name = "Gitignore",
    },
  };
  -- same as `override` but specifically for overrides by extension
  -- takes effect when `strict` is true
  override_by_extension = {
    ["norg"] = {
      icon = "",
      color = "#71a6e2",
      name = "norg",
    }
  },
}
end

return {
  "nvim-tree/nvim-web-devicons", lazy = true,
  enabled = vim.fn.has("nvim-0.7") == 1,
  config = function() require('plugins/icons') end,
  pin = true,
  commit = 'cde67b5d5427daeecfd7c77cf02ded23a26980bb',
}
